(ns cliente-healthy-life.core
  (:require [clj-http.client :as client]
            [cheshire.core :as json]
            [clojure.string :as str]))

(def base-url "http://localhost:3000")

;; ======== UTILITÁRIOS ========

(defn ler-linha-trim []
  (str/trim (read-line)))

(defn ler-double [msg]
  (try
    (print msg) (flush)
    (Double/parseDouble (ler-linha-trim))
    (catch Exception _
      (println "⚠ Entrada inválida! Digite um número.")
      nil)))

(defn ler-data [msg]
  (print msg) (flush)
  (let [input (ler-linha-trim)]
    (if (re-matches #"\d{4}-\d{2}-\d{2}" input)
      input
      (do (println "⚠ Formato inválido! Use AAAA-MM-DD.") (recur msg)))))

(defn menu-loop? [msg]
  (print (str msg " (s = sim / n = não): ")) (flush)
  (= "s" (str/lower-case (ler-linha-trim))))

(defn escolher-item [itens label-fn]
  (doseq [[i item] (map-indexed vector itens)]
    (println (format "%d. %s" i (label-fn item))))
  (print "Selecione o número desejado: ") (flush)
  (let [input (ler-linha-trim)
        idx   (try (Integer/parseInt input) (catch Exception _ -1))]
    (if (and (>= idx 0) (< idx (count itens)))
      (nth itens idx)
      (do (println "⚠ Seleção inválida!") (recur itens label-fn)))))

;; ======== CHAMADAS À API ========

(defn buscar-alimentos [termo]
  (let [response (client/get (str base-url "/buscar-alimentos")
                             {:query-params {"termo" termo}
                              :as :json
                              :throw-exceptions false})]
    (if (= 200 (:status response))
      (get-in response [:body :alimentos])
      (do (println (format "⚠ Erro: %s - %s" (:status response) (get-in response [:body :detalhes])))
          []))))

(defn obter-kcal-100g [fdc-id]
  (let [response (client/get (str base-url "/calorias-100g/" fdc-id)
                             {:as :json
                              :throw-exceptions false})]
    (if (= 200 (:status response))
      (get-in response [:body :kcal-por-100g])
      (do (println (format "⚠ Erro ao obter kcal: %s - %s" (:status response) (get-in response [:body :detalhes])))
          nil))))

(defn salvar-usuario! [usuario]
  (let [response (client/post (str base-url "/salvar-usuario")
                              {:body (json/generate-string usuario)
                               :content-type :json
                               :as :json
                               :throw-exceptions false})]
    (if (= 200 (:status response))
      (println "✅ Usuário cadastrado com sucesso!")
      (println (format "⚠ Erro: %s - %s" (:status response) (get-in response [:body :detalhes]))))))

(defn adicionar-alimento! [alimento]
  (let [response (client/post (str base-url "/adicionar-alimento")
                              {:body (json/generate-string alimento)
                               :content-type :json
                               :as :json
                               :throw-exceptions false})]
    (if (= 200 (:status response))
      (println "✅ Alimento registrado!")
      (println (format "⚠ Erro: %s - %s" (:status response) (get-in response [:body :detalhes]))))))

(defn adicionar-exercicio! [exercicio]
  (let [response (client/post (str base-url "/adicionar-exercicio")
                              {:body (json/generate-string exercicio)
                               :content-type :json
                               :as :json
                               :throw-exceptions false})]
    (if (= 200 (:status response))
      (println "✅ Exercício registrado!")
      (println (format "⚠ Erro: %s - %s" (:status response) (get-in response [:body :detalhes]))))))

(defn obter-dados []
  (let [response (client/get (str base-url "/obter-dados")
                             {:as :json
                              :throw-exceptions false})]
    (if (= 200 (:status response))
      (:body response)
      (do (println (format "⚠ Erro: %s - %s" (:status response) (get-in response [:body :detalhes])))
          {}))))

(defn escolher-periodo []
  (let [response (client/get (str base-url "/datas-disponiveis")
                             {:as :json
                              :throw-exceptions false})]
    (if (= 200 (:status response))
      (let [datas (get-in response [:body :datas])]
        (if (empty? datas)
          (do (println "⚠ Não há dados disponíveis para gerar relatório.") nil)
          (do
            (println "\n📅 Datas disponíveis:")
            (doseq [data datas] (println " - " data))
            (println "\nDefina o período para o relatório:")
            {:inicio (ler-data "Data inicial (AAAA-MM-DD): ")
             :fim    (ler-data "Data final (AAAA-MM-DD): ")})))
      (do (println (format "⚠ Erro: %s - %s" (:status response) (get-in response [:body :detalhes])))
          nil))))

;; ======== FUNÇÕES DE MENU ========

(defn cadastrar-usuario []
  (print "Nome: ") (flush)
  (let [nome (ler-linha-trim)
        peso (ler-double "Peso (kg): ")]
    (if (and (not (str/blank? nome)) peso)
      (salvar-usuario! {:nome nome :peso peso})
      (println "⚠ Dados inválidos!"))))

(defn adicionar-alimento []
  (print "Data do consumo (AAAA-MM-DD): ") (flush)
  (let [data (ler-linha-trim)
        termo (do (print "Nome do alimento: ") (flush) (ler-linha-trim))
        alimentos (buscar-alimentos termo)]
    (if (empty? alimentos)
      (println "⚠ Nenhum alimento encontrado.")
      (let [itens (map #(assoc % :kcal100g (obter-kcal-100g (:fdcId %))) alimentos)
            escolhido (escolher-item itens #(format "%s - %.1f kcal/100g" (:description %) (:kcal100g %)))
            gramas (ler-double "Quantos gramas ingeridos? ")]
        (when gramas
          (let [info {:descricao (:description escolhido)
                      :fdcId (:fdcId escolhido)
                      :gramas gramas
                      :kcal (Math/round (* (/ (:kcal100g escolhido) 100.0) gramas))
                      :data data}]
            (adicionar-alimento! info)
            (println (format "\n🍽 %s | %.1fg | %d kcal" (:descricao info) gramas (:kcal info)))
            (when (menu-loop? "Deseja adicionar outro alimento?")
              (adicionar-alimento))))))))

(defn adicionar-exercicio []
  (print "Data do exercício (AAAA-MM-DD): ") (flush)
  (let [data (ler-linha-trim)
        nome (do (print "Nome do exercício: ") (flush) (ler-linha-trim))
        duracao (ler-double "Duração (min): ")
        peso (:peso (first (vals (:usuarios (obter-dados)))))]
    (if (and (not (str/blank? nome)) duracao peso)
      (let [response (client/get (str base-url "/atividade")
                                 {:query-params {"atividade" nome
                                                 "peso" peso
                                                 "duracao" duracao}
                                  :as :json
                                  :throw-exceptions false})]
        (if (= 200 (:status response))
          (let [atividades (get-in response [:body :variantes])]
            (if (empty? atividades)
              (println "⚠ Nenhum exercício encontrado.")
              (let [escolhido (escolher-item atividades #(format "%s - %s kcal" (:name %) (:total_calories %)))
                    exercicio {:nome (:name escolhido)
                               :duracao (:duration_minutes escolhido)
                               :calorias (:total_calories escolhido)
                               :data data}]
                (adicionar-exercicio! exercicio)
                (println (format "\n🏃 %s | %s min | %s kcal" (:nome exercicio) (:duracao exercicio) (:calorias exercicio)))
                (when (menu-loop? "Deseja adicionar outro exercício?")
                  (adicionar-exercicio)))))
          (println (format "⚠ Erro: %s - %s" (:status response) (get-in response [:body :detalhes])))))
      (println "⚠ Dados incompletos!"))))

(defn mostrar-relatorio []
  (if-let [periodo (escolher-periodo)]
    (let [{:keys [inicio fim]} periodo
          dados (obter-dados)
          alimentos-filtrados (filter #(and (>= (compare (:data %) inicio) 0)
                                            (<= (compare (:data %) fim) 0))
                                      (:alimentos dados))
          exercicios-filtrados (filter #(and (>= (compare (:data %) inicio) 0)
                                             (<= (compare (:data %) fim) 0))
                                       (:exercicios dados))
          total-alimentos (reduce + (map :kcal alimentos-filtrados))
          total-exercicios (reduce + (map :calorias exercicios-filtrados))
          saldo (- total-alimentos total-exercicios)]
      (println (format "\n=== 📊 Relatório de Calorias (%s a %s) ===" inicio fim))
      (println "\n👤 Usuário:")
      (doseq [[nome usuario] (:usuarios dados)]
        (println (format " - %s (%.1f kg)" nome (:peso usuario))))
      (println "\n🍽 Alimentos Consumidos:")
      (doseq [alimento alimentos-filtrados]
        (println (format " - [%s] %s: %d kcal" (:data alimento) (:descricao alimento) (:kcal alimento))))
      (println (format "\n🔴 Total de calorias consumidas: %d kcal" total-alimentos))
      (println "\n🏋 Exercícios Realizados:")
      (doseq [exercicio exercicios-filtrados]
        (println (format " - [%s] %s: %s kcal" (:data exercicio) (:nome exercicio) (:calorias exercicio))))
      (println (format "\n⚖ Saldo de calorias: %s%d kcal"
                       (if (neg? saldo) "" "+")
                       saldo)))
    (println "Operação cancelada.")))

;; ======== MENU PRINCIPAL ========

(defn mostrar-menu []
  (println "\n======== 🥗 MENU 🏋 ========")
  (println "1. Cadastrar Informações")
  (println "2. Adicionar Alimento")
  (println "3. Adicionar Exercício")
  (println "4. Relatório de Calorias")
  (println "0. Sair")
  (print "Escolha uma opção: ") (flush))

(defn -main []
  (loop []
    (mostrar-menu)
    (case (ler-linha-trim)
      "1" (do (cadastrar-usuario) (recur))
      "2" (do (adicionar-alimento) (recur))
      "3" (do (adicionar-exercicio) (recur))
      "4" (do (mostrar-relatorio) (recur))
      "0" (println "👋 Até logo!")
      (do (println "⚠ Opção inválida.") (recur)))))
