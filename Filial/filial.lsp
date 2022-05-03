(in-package :user)

(defstruct Filial
   id
   nome
   descricao
   endereco
   departamentos
)

(setq filiais (list ))

(defun mainFilial()
    (loop
        (terpri)
        (princ "-------------------") (terpri) 
        (princ "=== Filial ===") (terpri)
        (princ "-------------------") (terpri)
        (terpri)
        (princ "1.. Criar") (terpri)
        (princ "2.. Consultar") (terpri)
        (princ "3.. Atualizar") (terpri)
        (princ "4.. Deletar") (terpri)
        (princ "5.. Listar todos") (terpri)
        (princ "6.. Departamento") (terpri)
        (princ "7.. Sair") (terpri)
        (princ "Escolha uma opção acima: ")

        (setq opFilial (read))
        
        (cond
            ((= opFilial 1)
                (princ "---------------") (terpri) 
                (princ "     CRIAR     ") (terpri)
                (princ "---------------") (terpri)

                (princ "id: ")
                (setq id (read-line))
                (princ "Nome: ")
                (setq nome (read-line))
                (princ "descricao: ")
                (setq descricao (read-line))
                (princ "Endereço: ")
                (setq ende (read-line))

                ( setq filial (make-Filial
                    :id id
                    :nome nome
                    :descricao descricao
                    :endereco ende
                    :departamentos (list )
                    )
                )
                (if ( = 0 (length filiais))
                    (push filial filiais)
                    (push filial (cdr (last filiais)))
                )  
            )
            ((and (= opFilial 2) (> (length filiais) 0)) 

                (loop

                    (princ "---------------") (terpri)
                    (princ "   CONSULTAR   ") (terpri)
                    (princ "---------------") (terpri)

                    (listarFiliais)
                    (setq idEscolhido (read))

                    (when (or (< idEscolhido 0) (>= idEscolhido (length filiais)))
                        (terpri)(terpri)
                        (princ "ERROR!!! Opção INVÁLIDA!!") (terpri)
                        (princ "ERROR!!! ID não encontrado!!") (terpri)
                        (return)
                    )

                    (detalheFilial idEscolhido)
                    (princ "Deseja fazer outra consulta? (S/N) ")
                    (setq novaConsul (read-char))

                    (when (and (char/= novaConsul #\s) (char/= novaConsul #\S))
                        (return)
                    )
                )
            )
            ((and (= opFilial 3) (> (length filiais) 0)) 
                (loop

                    (princ "---------------") (terpri)
                    (princ "   ATUALIZAR   ") (terpri)
                    (princ "---------------") (terpri)

                    (listarFiliais)
                    (setq idEscolhido (read))

                    (when (or (< idEscolhido 0) (>= idEscolhido (length filiais)))
                        (terpri)(terpri)
                        (princ "ERROR!!! Opção INVÁLIDA!!") (terpri)
                        (princ "ERROR!!! ID não encontrado!!") (terpri)
                        (return)
                    )

                    (detalheFilial idEscolhido)
                    (princ "Tem certeza que deseja alterar ? (S/N) ")
                    (setq confirm (read-char))

                    (when (or (char= confirm #\s) (char= confirm #\S))
                        (setq func (nth idEscolhido filiais))
                        
                        (clear-input)
                        (princ "id: ")
                        (setq id (read-line))
                        (princ "Nome: ")
                        (setq nome (read-line))
                        (princ "descricao: ")
                        (setq descricao (read-line))
                        (princ "Endereço: ")
                        (setq ende (read-line))

                        (setf (Filial-id func) id)
                        (setf (Filial-nome func) nome)
                        (setf (Filial-descricao func) descricao)
                        (setf (Filial-endereco func) ende)

                        (terpri)
                        (princ "Alterado com SUCESSO!!!")
                        (terpri)
                    )

                    (return)
                )
            )
            ((and (= opFilial 4) (> (length filiais) 0))   
                (loop
                    (princ "---------------") (terpri)
                    (princ "    DELETAR    ") (terpri)
                    (princ "---------------") (terpri)

                    (listarFiliais)
                    (setq idEscolhido (read))

                    (when (or (< idEscolhido 0) (>= idEscolhido (length filiais)))
                        (terpri)(terpri)
                        (princ "ERROR!!! Opção INVÁLIDA!!") (terpri)
                        (princ "ERROR!!! ID não encontrado!!") (terpri)
                        (return)
                    )

                    (detalheFilial idEscolhido)
                    (princ "Tem certeza que deseja deletar ? (S/N) ")
                    (setq confirm (read-char))

                    (when (or (char= confirm #\s) (char= confirm #\S))
                        (setq func (nth idEscolhido filiais))
                        
                        (if (= idEscolhido 0)
                            (pop filiais)
                            (delete func filiais)
                        )

                        (terpri)
                        (princ "Deletado com SUCESSO!!!")
                        (terpri)
                    )

                    (return)
                )

            )
            ((and (= opFilial 5) (> (length filiais) 0)) 
                (princ "---------------") (terpri)
                (princ "  LISTA TODOS  ") (terpri)
                (princ "---------------") (terpri)

                (princ "=============================")(terpri)
                (dolist (i filiais)
                    (format t "id                    : ~d ~%" (Filial-id i))
                    (format t "Nome                   : ~d ~%" (Filial-nome i))
                    (format t "descricao               : ~d ~%" (Filial-descricao i))
                    (format t "Endereço               : ~d ~%" (Filial-endereco i))
                    (format t "Quantidade de departamentos : ~d ~%" (length (Filial-departamentos i)))

(if(>=(length(Filial-departamentos filial))1)
(depFilial filial)
)
                )
                (terpri)
            )
            ((and (= opFilial 6) (> (length filiais) 0))
                (princ "---------------") (terpri)
                (princ "  DEPARTAMENTOS  ") (terpri)
                (princ "---------------") (terpri)

                (listarFiliais)
                (setq idEscolhido (read))

                (when (or (< idEscolhido 0) (>= idEscolhido (length filiais)))
                    (terpri)(terpri)
                    (princ "ERROR!!! Opção INVÁLIDA!!") (terpri)
                    (princ "ERROR!!! ID não encontrado!!") (terpri)
                    (return)
                )

                (mainDepartamento idEscolhido)
            )
            ((= opFilial 7) 
                (princ "--------------") (terpri)
                (princ "     SAIR     ") (terpri)
                (princ "--------------") (terpri)
                (princ "Saindo........") (terpri)
                (return)
            )
            ((or (< opFilial 1) (> opFilial 7)) 
                (terpri)                    
                (princ "ERROR!!! Opção INVÁLIDA!!") (terpri)
            )
            ((<= (length filiais) 0)
                (terpri)                    
                (princ "ERROR!!! A lista está vazia!!") (terpri)
            )
        )
    )
)

(defun listarFiliais ()
    (setq index 0)
    (terpri)
    (princ "-----------------------------")
    (dolist (i filiais)
        (format t "~% ID: ~d.... NomeF: ~d" index (Filial-nome i))
        (incf index)
    )
    (terpri)
    (princ "-----------------------------")(terpri)
    (princ "Escolha o ID da filial: ")
)

(defun detalheFilial (id)
    (setq filial (nth id filiais))
    (terpri)(terpri)
    (princ "=============================")(terpri)
    (format t "id                    : ~d ~%" (Filial-id filial))
    (format t "Nome                   : ~d ~%" (Filial-nome filial))
    (format t "descricao               : ~d ~%" (Filial-descricao filial))
    (format t "Endereço               : ~d ~%" (Filial-endereco filial))
    (format t "Quantidade de departamentos : ~d ~%" (length (Filial-departamentos filial)))
    (terpri)

(if(>=(length(Filial-departamentos filial))1)
(depFilial filial)
)
    (princ "=============================")(terpri)(terpri)
)

(defun depFilial(filial)
(princ"=== Departamento(s) ===")
(terpri)
(terpri)
    (setq deps (Filial-departamentos filial))

   (dolist (i deps)

    (format t "Departamento : ~d ~%" (Departamento-nome i))
    (terpri)
    )

    (terpri)

)

    
