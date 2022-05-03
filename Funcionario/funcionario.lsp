(in-package :user)

(defstruct Funcionario
   cpf
   nome
   telefone
   endereco
   listDepen
)

(setq listFunc (list ))

(defun mainFuncionario()
    (loop
        (terpri)
        (princ "-------------------") (terpri) 
        (princ "=== FUNCIONÁRIO ===") (terpri)
        (princ "-------------------") (terpri)

        (terpri)
        (princ "1.. Criar") (terpri)
        (princ "2.. Consultar") (terpri)
        (princ "3.. Atualizar") (terpri)
        (princ "4.. Deletar") (terpri)
        (princ "5.. Listar todos") (terpri)
        (princ "6.. Dependentes") (terpri)
        (princ "7.. Sair") (terpri)
        (princ "Escolha uma opção acima: ")

        (setq opFunc (read))
        
        (cond
            ((= opFunc 1)
                (princ "---------------") (terpri) 
                (princ "     CRIAR     ") (terpri)
                (princ "---------------") (terpri)

                (princ "CPF: ")
                (setq cpf (read-line))
                (princ "Nome: ")
                (setq nome (read-line))
                (princ "Telefone: ")
                (setq tel (read-line))
                (princ "Endereço: ")
                (setq ende (read-line))

                ( setq func (make-Funcionario
                    :cpf cpf
                    :nome nome
                    :telefone tel
                    :endereco ende
                    :listDepen (list )
                    )
                )
                (if ( = 0 (length listFunc))
                    (push func listFunc)
                    (push func (cdr (last listFunc)))
                )  
            )
            ((and (= opFunc 2) (> (length listFunc) 0)) 

                (loop

                    (princ "---------------") (terpri)
                    (princ "   CONSULTAR   ") (terpri)
                    (princ "---------------") (terpri)

                    (listarFunc)
                    (setq idEscolhido (read))

                    (when (or (< idEscolhido 0) (>= idEscolhido (length listFunc)))
                        (terpri)(terpri)
                        (princ "ERROR!!! Opção INVÁLIDA!!") (terpri)
                        (princ "ERROR!!! ID não encontrado!!") (terpri)
                        (return)
                    )

                    (detalheFunc idEscolhido)
                    (princ "Deseja fazer outra consulta? (S/N) ")
                    (setq novaConsul (read-char))

                    (when (and (char/= novaConsul #\s) (char/= novaConsul #\S))
                        (return)
                    )
                )
            )
            ((and (= opFunc 3) (> (length listFunc) 0)) 
                (loop

                    (princ "---------------") (terpri)
                    (princ "   ATUALIZAR   ") (terpri)
                    (princ "---------------") (terpri)

                    (listarFunc)
                    (setq idEscolhido (read))

                    (when (or (< idEscolhido 0) (>= idEscolhido (length listFunc)))
                        (terpri)(terpri)
                        (princ "ERROR!!! Opção INVÁLIDA!!") (terpri)
                        (princ "ERROR!!! ID não encontrado!!") (terpri)
                        (return)
                    )

                    (detalheFunc idEscolhido)
                    (princ "Tem certeza que deseja alterar ? (S/N) ")
                    (setq confirm (read-char))

                    (when (or (char= confirm #\s) (char= confirm #\S))
                        (setq func (nth idEscolhido listFunc))
                        
                        (clear-input)
                        (princ "CPF: ")
                        (setq cpf (read-line))
                        (princ "Nome: ")
                        (setq nome (read-line))
                        (princ "Telefone: ")
                        (setq tel (read-line))
                        (princ "Endereço: ")
                        (setq ende (read-line))

                        (setf (Funcionario-cpf func) cpf)
                        (setf (Funcionario-nome func) nome)
                        (setf (Funcionario-telefone func) tel)
                        (setf (Funcionario-endereco func) ende)

                        (terpri)
                        (princ "Alterado com SUCESSO!!!")
                        (terpri)
                    )

                    (return)
                )
            )
            ((and (= opFunc 4) (> (length listFunc) 0))   
                (loop
                    (princ "---------------") (terpri)
                    (princ "    DELETAR    ") (terpri)
                    (princ "---------------") (terpri)

                    (listarFunc)
                    (setq idEscolhido (read))

                    (when (or (< idEscolhido 0) (>= idEscolhido (length listFunc)))
                        (terpri)(terpri)
                        (princ "ERROR!!! Opção INVÁLIDA!!") (terpri)
                        (princ "ERROR!!! ID não encontrado!!") (terpri)
                        (return)
                    )

                    (detalheFunc idEscolhido)
                    (princ "Tem certeza que deseja deletar ? (S/N) ")
                    (setq confirm (read-char))

                    (when (or (char= confirm #\s) (char= confirm #\S))
                        (setq func (nth idEscolhido listFunc))
                        
                        (if (= idEscolhido 0)
                            (pop listFunc)
                            (delete func listFunc)
                        )

                        (terpri)
                        (princ "Deletado com SUCESSO!!!")
                        (terpri)
                    )

                    (return)
                )

            )
            ((and (= opFunc 5) (> (length listFunc) 0)) 
                (princ "---------------") (terpri)
                (princ "  LISTA TODOS  ") (terpri)
                (princ "---------------") (terpri)

                (princ "=============================")(terpri)
                (dolist (i listFunc)
                    (format t "CPF                    : ~d ~%" (Funcionario-cpf i))
                    (format t "Nome                   : ~d ~%" (Funcionario-nome i))
                    (format t "Telefone               : ~d ~%" (Funcionario-telefone i))
                    (format t "Endereço               : ~d ~%" (Funcionario-endereco i))
                    (format t "Quantidade dependentes : ~d ~%" (length (Funcionario-listDepen i)))
                    (princ "=============================")(terpri)
                )
                (terpri)
            )
            ((and (= opFunc 6) (> (length listFunc) 0))
                (princ "---------------") (terpri)
                (princ "  DEPENDENTES  ") (terpri)
                (princ "---------------") (terpri)

                (listarFunc)
                (setq idEscolhido (read))

                (when (or (< idEscolhido 0) (>= idEscolhido (length listFunc)))
                    (terpri)(terpri)
                    (princ "ERROR!!! Opção INVÁLIDA!!") (terpri)
                    (princ "ERROR!!! ID não encontrado!!") (terpri)
                    (return)
                )

                (mainDependente idEscolhido)
            )
            ((= opFunc 7) 
                (princ "--------------") (terpri)
                (princ "     SAIR     ") (terpri)
                (princ "--------------") (terpri)
                (princ "Saindo........") (terpri)
                (return)
            )
            ((or (< opFunc 1) (> opFunc 7)) 
                (terpri)                    
                (princ "ERROR!!! Opção INVÁLIDA!!") (terpri)
            )
            ((<= (length listFunc) 0)
                (terpri)                    
                (princ "ERROR!!! A lista está vazia!!") (terpri)
            )
        )
    )
)

(defun listarFunc ()
    (setq index 0)
    (terpri)
    (princ "-----------------------------")
    (dolist (i listFunc)
        (format t "~% ID: ~d.... Nome: ~d" index (Funcionario-nome i))
        (incf index)
    )
    (terpri)
    (princ "-----------------------------")(terpri)
    (princ "Escolha o ID do funcionário: ")
)

(defun detalheFunc (id)

    (setq Func (nth id listFunc))
    (terpri)(terpri)
    (princ "=============================")(terpri)
    (format t "CPF                    : ~d ~%" (Funcionario-cpf Func))
    (format t "Nome                   : ~d ~%" (Funcionario-nome Func))
    (format t "Telefone               : ~d ~%" (Funcionario-telefone Func))
    (format t "Endereço               : ~d ~%" (Funcionario-endereco Func))
    (format t "Quantidade dependentes : ~d ~%" (length (Funcionario-listDepen Func)))
    (princ "=============================")(terpri)(terpri)

)



