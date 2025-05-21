(ns cliente-healthy-life.core
  (:require [clj-http.client :as client]
            [cheshire.core :as json]
            [clojure.string :as str]))

(def base-url "http://localhost:3000")

(defonce usuario (atom {}))
(defonce alimentos-consumidos (atom []))
(defonce exercicios-executados (atom []))

;; ======== Utilitários ========
(defn ler-linha-trim []
  (str/trim (read-line)))

(defn ler-double [msg]
  (try
    (print msg) (flush)
    (Double/parseDouble (ler-linha-trim))
    (catch Exception _
      (println "⚠ Entrada inválida! Digite um número.")
      nil)))

(defn menu-loop? [msg]
  (print (str msg " (s = sim / outro = não): "))
  (flush)
  (= "s" (str/lower-case (ler-linha-trim))))

(defn escolher-item [itens label-fn]
  (doseq [[i item] (map-indexed vector itens)]
    (println (format "%d. %s" i (label-fn item))))
  (print "Selecione o número desejado: ")
  (flush)
  (let [input (ler-linha-trim)
        idx   (try (Integer/parseInt input) (catch Exception _ -1))]
    (if (and (>= idx 0) (< idx (count itens)))
      (nth itens idx)
      (do (println "⚠ Seleção inválida!") (recur itens label-fn)))))

;; ======== Chamadas à API ========
(defn buscar-alimentos [termo]
  (let [response (client/get (str base-url "/buscar-alimentos")
                             {:query-params {"termo" termo}
                              :as :json})]
    (get-in response [:body :alimentos])))

(defn obter-kcal-100g [fdc-id]
  "Obtém kcal para 100 g via POST /calcular-calorias no backend."
  (let [payload  {:fdcId  fdc-id
                  :gramas 100}
        response (client/post (str base-url "/calcular-calorias")
                              {:body            (json/generate-string payload)
                               :headers         {"Content-Type" "application/json"}
                               :throw-exceptions false
                               :as              :json})
        corpo     (:body response)]
    (or (get corpo :kcal)
        (do (println "⚠ Erro: não foi possível obter kcal/100g para fdcId=" fdc-id)
            0.0))))

(defn buscar-atividades [atividade peso duracao]
  (let [params   {"atividade" atividade
                  "peso"      (str peso)
                  "duracao"   (str duracao)}
        response (client/get (str base-url "/atividade")
                             {:query-params params :as :json})]
    (get-in response [:body :variantes])))

;; ======== Funções de Menu ========
(defn cadastrar-usuario []
  (print "Nome: ") (flush)
  (let [nome (ler-linha-trim)
        peso (ler-double "Peso (kg): ")]
    (if (and (not (str/blank? nome)) peso)
      (do (reset! usuario {:nome nome :peso peso})
          (println "✅ Usuário cadastrado com sucesso!"))
      (println "⚠ Dados inválidos!"))))

(defn adicionar-alimento []
  (print "Data do consumo (AAAA-MM-DD): ") (flush)
  (let [data      (ler-linha-trim)
        _         (do (print "Nome do alimento: ") (flush))
        termo     (ler-linha-trim)
        alimentos (buscar-alimentos termo)
        itens     (map (fn [a]
                         (let [k100 (obter-kcal-100g (:fdcId a))]
                           (assoc a :kcal100g k100)))
                       alimentos)]
    (if (empty? itens)
      (println "⚠ Nenhum alimento encontrado.")
      (let [escolhido (escolher-item itens
                                     #(format "%s - %.1f kcal/100g"
                                              (:description %) (:kcal100g %)))
            gramas    (ler-double "Quantos gramas ingeridos? ")]
        (if (and gramas (:kcal100g escolhido))
          (let [total (long (Math/round (* (/ (:kcal100g escolhido) 100.0) gramas)))
                info  {:descricao (:description escolhido)
                       :fdcId      (:fdcId escolhido)
                       :gramas     gramas
                       :kcal       total
                       :data       data}]
            (println (format "\n🍽 Alimento: %s | Gramas: %.1f | Total kcal: %d"
                             (:descricao info) (:gramas info) (:kcal info)))
            (swap! alimentos-consumidos conj info)
            (when (menu-loop? "Deseja adicionar outro alimento?")
              (adicionar-alimento)))
          (println "⚠ Quantidade ou kcal desconhecido."))))))

(defn adicionar-exercicio []
  (print "Data do exercício (AAAA-MM-DD): ") (flush)
  (let [data       (ler-linha-trim)
        _          (do (print "Nome do exercício: ") (flush))
        nome       (ler-linha-trim)
        duracao    (ler-double "Duração (min): ")
        peso       (:peso @usuario)
        atividades (when (and nome duracao peso)
                     (buscar-atividades nome peso duracao))]
    (if (or (empty? atividades) (nil? atividades))
      (println "⚠ Nenhum exercício encontrado.")
      (let [escolhido (escolher-item atividades
                                     #(format "%s - %s kcal"
                                              (:name %) (:total_calories %)))
            exercicio {:nome     (:name escolhido)
                       :duracao  (:duration_minutes escolhido)
                       :calorias (:total_calories escolhido)
                       :data     data}]
        (println (format "\n🏃 Exercício: %s | Duração: %s min | Calorias: %s kcal"
                         (:nome exercicio) (:duracao exercicio) (:calorias exercicio)))
        (swap! exercicios-executados conj exercicio)
        (when (menu-loop? "Deseja adicionar outro exercício?")
          (adicionar-exercicio))))))

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
      "4" (do (println "🔧 Relatório ainda em desenvolvimento.") (recur))
      "0" (println "👋 Até logo!")
      (do (println "⚠ Opção inválida.") (recur)))))
