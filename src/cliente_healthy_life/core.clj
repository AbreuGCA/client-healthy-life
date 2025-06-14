(ns cliente-healthy-life.core
  (:require [clj-http.client :as client]
            [cheshire.core :as json]
            [clojure.string :as str]))

(def base-url "http://localhost:3000")

;; ======== UTILITÁRIOS ========

(defn ler-linha-trim []
  (str/trim (read-line)))

(defn ler-sexo []
  (print "Sexo (M/F): ") (flush)
  (let [entrada (str/upper-case (ler-linha-trim))]
    (if (contains? #{"M" "F"} entrada)
      entrada
      (do
        (println "⚠ Sexo inválido! Digite M ou F.")
        (recur)))))

(defn numero-valido? [s]
  (try
    (pos? (Double/parseDouble s))
    (catch Exception _ false)))

(defn ler-double [msg]
  (print msg) (flush)
  (let [entrada (ler-linha-trim)]
    (if (numero-valido? entrada)
      (Double/parseDouble entrada)
      (do
        (println "⚠ Erro, entrada inválida!")
        (recur msg)))))

(defn ler-data [msg]
  (print msg) (flush)
  (let [input (ler-linha-trim)]
    (if (re-matches #"\d{4}-\d{2}-\d{2}" input)
      input
      (do
        (println "⚠ Formato inválido! Use AAAA-MM-DD.")
        (recur msg)))))

(defn menu-loop? [msg]
  (print (str msg " (s = sim / n = não): ")) (flush)
  (= "s" (str/lower-case (ler-linha-trim))))

(defn imprimir-itens [itens label-fn]
  (doall
    (map println
         (map-indexed
           (fn [i item]
             (format "%d. %s" i (label-fn item)))
           itens))))

(defn escolher-item [itens label-fn]
  (imprimir-itens itens label-fn)
  (print "Selecione o número desejado: ") (flush)
  (let [input (ler-linha-trim)
        idx   (try (Integer/parseInt input) (catch Exception _ -1))]
    (if (and (>= idx 0) (< idx (count itens)))
      (nth itens idx)
      (do
        (println "⚠ Seleção inválida!")
        (recur itens label-fn)))))

;; ======== CHAMADAS À API ========

(defn buscar-alimentos [termo]
  (println (format "🔍 Buscando resultados para %s... aguarde." termo))
  (let [response (client/get (str base-url "/buscar-alimentos")
                             {:query-params {"termo" termo}
                              :as            :json
                              :throw-exceptions false})]
    (if (= 200 (:status response))
      (get-in response [:body :alimentos])
      (do
        (println (format "⚠ Erro: %s - %s"
                         (:status response)
                         (get-in response [:body :detalhes])))
        []))))

(defn obter-kcal-100g [fdc-id]
  (let [response (client/get (str base-url "/calorias-100g/" fdc-id)
                             {:as            :json
                              :throw-exceptions false})]
    (if (= 200 (:status response))
      (get-in response [:body :kcal-por-100g])
      (do
        (println (format "⚠ Erro ao obter kcal: %s - %s"
                         (:status response)
                         (get-in response [:body :detalhes])))
        nil))))

(defn buscar-exercicios [nome peso duracao]
  (println (format "🔍 Buscando resultados para %s... aguarde." nome))
  (let [response (client/get (str base-url "/atividade")
                             {:query-params {"atividade" nome
                                             "peso"      peso
                                             "duracao"   duracao}
                              :as                 :json
                              :throw-exceptions   false})]
    (if (= 200 (:status response))
      (get-in response [:body :variantes])
      (do
        (println (format "⚠ Erro: %s - %s"
                         (:status response)
                         (get-in response [:body :detalhes])))
        []))))

(defn salvar-usuario! [usuario]
  (let [response (client/post (str base-url "/salvar-usuario")
                              {:body          (json/generate-string usuario)
                               :content-type  :json
                               :as            :json
                               :throw-exceptions false})]
    (if (= 200 (:status response))
      (println "✅ Usuário cadastrado com sucesso!")
      (println (format "⚠ Erro: %s - %s"
                       (:status response)
                       (get-in response [:body :detalhes]))))))

(defn adicionar-alimento! [alimento]
  (let [response (client/post (str base-url "/adicionar-alimento")
                              {:body          (json/generate-string alimento)
                               :content-type  :json
                               :as            :json
                               :throw-exceptions false})]
    (if (= 200 (:status response))
      (println "✅ Alimento registrado!")
      (println (format "⚠ Erro: %s - %s"
                       (:status response)
                       (get-in response [:body :detalhes]))))))

(defn adicionar-exercicio! [exercicio]
  (let [response (client/post (str base-url "/adicionar-exercicio")
                              {:body          (json/generate-string exercicio)
                               :content-type  :json
                               :as            :json
                               :throw-exceptions false})]
    (if (= 200 (:status response))
      (println "✅ Exercício registrado!")
      (println (format "⚠ Erro: %s - %s"
                       (:status response)
                       (get-in response [:body :detalhes]))))))

(defn obter-dados []
  (let [response (client/get (str base-url "/obter-dados")
                             {:as            :json
                              :throw-exceptions false})]
    (if (= 200 (:status response))
      (:body response)
      (do
        (println (format "⚠ Erro: %s - %s"
                         (:status response)
                         (get-in response [:body :detalhes])))
        {}))))

(defn usuario-cadastrado? []
  (try
    (let [response (client/get (str base-url "/obter-dados")
                               {:as            :json
                                :throw-exceptions false})
          usuarios (get-in response [:body :usuarios])]
      (seq usuarios))
    (catch Exception e
      (println "⚠ Erro ao verificar usuário:" (.getMessage e))
      false)))

(defn escolher-periodo []
  (let [response (client/get (str base-url "/datas-disponiveis")
                             {:as            :json
                              :throw-exceptions false})]
    (if (= 200 (:status response))
      (let [datas (get-in response [:body :datas])]
        (if (empty? datas)
          (do
            (println "⚠ Não há dados disponíveis para gerar relatório.")
            nil)
          (do
            (println "\n📅 Datas disponíveis:")
            (doall (map #(println " - " %) datas))
            (println "\nDefina o período para o relatório:")
            {:inicio (ler-data "Data inicial (AAAA-MM-DD): ")
             :fim    (ler-data "Data final (AAAA-MM-DD): ")})))
      (do
        (println (format "⚠ Erro: %s - %s"
                         (:status response)
                         (get-in response [:body :detalhes])))
        nil))))

;; ======== FUNÇÕES DE MENU ========

(defn cadastrar-usuario []
  (print "Nome: ") (flush)
  (let [nome   (ler-linha-trim)]
    (if (str/blank? nome)
      (do
        (println "⚠ Nome inválido!")
        (recur))
      (let [peso   (ler-double "Peso (kg): ")
            altura (ler-double "Altura (cm): ")
            idade  (ler-double "Idade: ")
            sexo   (ler-sexo)]
        (salvar-usuario! {:nome   nome
                          :peso   peso
                          :altura altura
                          :idade  idade
                          :sexo   sexo})))))

(defn adicionar-alimento []
  (letfn [(loop-alimentos []
            (let [data      (ler-data "Data do consumo (AAAA-MM-DD): ")
                  termo     (do (print "Nome do alimento: ") (flush) (ler-linha-trim))
                  alimentos (buscar-alimentos termo)]
              (if (empty? alimentos)
                (println "⚠ Nenhum alimento encontrado.")
                (let [itens     (map #(assoc % :kcal100g (obter-kcal-100g (:fdcId %))) alimentos)
                      escolhido (escolher-item itens
                                               #(format "%s - %.1f kcal/100g"
                                                        (:description %)
                                                        (:kcal100g %)))
                      kcal100g  (:kcal100g escolhido)]
                  (cond
                    (= kcal100g -1)
                    (do (println "⚠ Unidade de energia desconhecida. Não é possível calcular calorias para esse alimento.")
                        (when (menu-loop? "Deseja tentar outro alimento?") (recur)))

                    (= kcal100g -2)
                    (do (println "⚠ Nenhuma informação de energia encontrada para esse alimento.")
                        (when (menu-loop? "Deseja tentar outro alimento?") (recur)))

                    :else
                    (let [gramas (ler-double "Quantos gramas ingeridos? ")]
                      (when gramas
                        (let [info {:descricao (:description escolhido)
                                    :fdcId     (:fdcId escolhido)
                                    :gramas    gramas
                                    :kcal      (Math/round (* (/ (:kcal100g escolhido) 100.0) gramas))
                                    :data      data}]
                          (adicionar-alimento! info)
                          (println (format "\n🍽 %s | %.1fg | %d kcal"
                                           (:descricao info)
                                           gramas
                                           (:kcal info)))
                          (when (menu-loop? "Deseja adicionar outro alimento?")
                            (recur))))))))))]
    (loop-alimentos)))

(defn adicionar-exercicio []
  (letfn [(loop-exercicios []
            (let [data    (ler-data "Data do exercício (AAAA-MM-DD): ")
                  nome    (do (print "Nome do exercício: ") (flush) (ler-linha-trim))
                  duracao (ler-double "Duração (min): ")
                  dados   (obter-dados)
                  peso    (when (seq (:usuarios dados))
                            (-> dados :usuarios first val :peso))
                  atividades (buscar-exercicios nome peso duracao)]
              (if (and (not (str/blank? nome)) duracao peso)
                (if (empty? atividades)
                  (println (str "⚠ Nenhum exercício encontrado para o termo: \"" nome "\""))
                  (let [escolhido (escolher-item atividades
                                                 #(format "%s - %s kcal"
                                                          (:name %)
                                                          (:total_calories %)))
                        exercicio {:nome      (:name escolhido)
                                   :duracao  (:duration_minutes escolhido)
                                   :calorias (:total_calories escolhido)
                                   :data     data}]
                    (adicionar-exercicio! exercicio)
                    (println (format "\n🏃 %s | %s min | %s kcal"
                                     (:nome exercicio)
                                     (:duracao exercicio)
                                     (:calorias exercicio)))))
                (println "⚠ Dados inválidos!"))
              (when (menu-loop? "Deseja adicionar outro exercício?")
                (recur))))]
    (loop-exercicios)))

(defn mostrar-relatorio []
  (if-let [periodo (escolher-periodo)]
    (let [{:keys [inicio fim]} periodo
          response        (client/get (str base-url "/extrato")
                                      {:query-params {"inicio" inicio
                                                      "fim"    fim}
                                       :as            :json
                                       :throw-exceptions false})]
      (if (= 200 (:status response))
        (let [{:keys [usuarios alimentos exercicios saldo consumido gasto]} (:body response)]
          (println (format "\n=== 📊 Relatório de Calorias (%s a %s) ===\n" inicio fim))
          (println "👤 Usuário:")
          (doall (map (fn [[nome usuario]]
                        (println (format " - %s: %.1f kg, %.0f cm, %d anos, sexo: %s"
                                         nome
                                         (:peso usuario)
                                         (:altura usuario)
                                         (int (:idade usuario))
                                         (:sexo usuario))))
                      usuarios))
          (println "\n🍽 Alimentos Consumidos:")
          (doall (map (fn [alimento]
                        (println (format " - [%s] %s: %d kcal - %.0f gramas"
                                         (:data alimento)
                                         (:descricao alimento)
                                         (:kcal alimento)
                                         (:gramas alimento))))
                      alimentos))
          (println (format "\n🔴 Total de calorias consumidas: %d kcal\n" consumido))
          (println "🏋 Exercícios Realizados:")
          (doall (map (fn [exercicio]
                        (println (format " - [%s] %s: %s kcal - %s minutos"
                                         (:data exercicio)
                                         (:nome exercicio)
                                         (:calorias exercicio)
                                         (:duracao exercicio))))
                      exercicios))
          (println (format "\n🔴 Total de calorias gastas: %d kcal\n" gasto))
          (println (format "⚖ Saldo de calorias: %s%d kcal"
                           (if (neg? saldo) "" "+")
                           saldo)))
        (println "⚠ Erro ao obter extrato.")))
    (println "Operação cancelada.")))

(defn mostrar-saldo []
  (if-let [periodo (escolher-periodo)]
    (let [{:keys [inicio fim]} periodo
          response        (client/get (str base-url "/saldo")
                                      {:query-params {"inicio" inicio
                                                      "fim"    fim}
                                       :as            :json
                                       :throw-exceptions false})]
      (if (= 200 (:status response))
        (let [{:keys [consumido gasto saldo]} (:body response)]
          (println (format "\n=== ⚖ Saldo de Calorias (%s a %s) ===" inicio fim))
          (println (format "🍽 Calorias consumidas: %d kcal" consumido))
          (println (format "🏋 Calorias gastas   : %d kcal" gasto))
          (println (format "🧮 Saldo final       : %s%d kcal"
                           (if (neg? saldo) "" "+")
                           saldo)))
        (println "⚠ Erro ao obter saldo.")))
    (println "Operação cancelada.")))

;; ======== MENU PRINCIPAL ========

(defn mostrar-menu []
  (println "\n======== 🥗 MENU 🏋 ========")
  (println "1. Adicionar Alimento")
  (println "2. Adicionar Exercício")
  (println "3. Relatório Detalhado de Calorias")
  (println "4. Exibir Saldo de Calorias")
  (println "0. Sair")
  (print "Escolha uma opção: ") (flush))

(defn -main []
  (if (usuario-cadastrado?)
    (println "\n✅ Usuário já cadastrado. Vamos continuar!\n")
    (do
      (println "\n👤 Nenhum usuário encontrado. Vamos cadastrá-lo agora:\n")
      (cadastrar-usuario)))

  ;; Loop principal
  (letfn [(menu-loop []
            (mostrar-menu)
            (case (ler-linha-trim)
              "1" (do (adicionar-alimento) (recur))
              "2" (do (adicionar-exercicio) (recur))
              "3" (do (mostrar-relatorio) (recur))
              "4" (do (mostrar-saldo) (recur))
              "0" (println "👋 Até logo!")
              (do (println "⚠ Opção inválida.") (recur))))]
    (menu-loop)))

(when (= *ns* 'cliente-healthy-life.core)
  (-main))
