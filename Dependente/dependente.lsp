(in-package :user)

(defstruct Dependente
   nome
   cpf
   sexo
   tipo
)

(defun mainDependente(idFunc)
    (setq func (nth idFunc listFunc))
    (setq listDepen (Funcionario-listDepen func))

    (loop
        (terpri)
        (princ "---------------") (terpri)
        (princ "  DEPENDENTES  ") (terpri)
        (princ "=== Funcionário responsável ===") (terpri)
        (format t "CPF  : ~d ~%" (Funcionario-cpf Func))
        (format t "Nome : ~d ~%" (Funcionario-nome Func))
        (princ "---------------") (terpri)

        (selectOption)
        (setq opDep (read))

        (cond
            ((= opDep 1)
                (princ "---------------") (terpri) 
                (princ "     CRIAR     ") (terpri)
                (princ "---------------") (terpri)

                (princ "Nome: ")
                (setq nome (read-line))
                (princ "CPF: ")
                (setq cpf (read-line))
                (princ "Sexo: ")
                (setq sexo (read-line))
                (princ "Tipo: ")
                (setq tipo (read-line))

                ( setq dep (make-Dependente
                    :nome nome
                    :cpf cpf
                    :sexo sexo
                    :tipo tipo
                )
                )
                (if ( = 0 (length listDepen))
                    (push dep listDepen)
                    (push dep (cdr (last listDepen)))
                )
            )
            ((and (= opDep 2) (> (length listDepen) 0)) 

                (loop

                    (princ "---------------") (terpri)
                    (princ "   CONSULTAR   ") (terpri)
                    (princ "---------------") (terpri)

                    (listarDepen listDepen)
                    (setq idEscolhido (read))

                    (when (or (< idEscolhido 0) (>= idEscolhido (length listDepen)))
                        (terpri)(terpri)
                        (princ "ERROR!!! Opção INVÁLIDA!!") (terpri)
                        (princ "ERROR!!! ID não encontrado!!") (terpri)
                        (return)
                    )

                    (detalheDepen idEscolhido listDepen)
                    (princ "Deseja fazer outra consulta? (S/N) ")
                    (setq novaConsul (read-char))

                    (when (and (char/= novaConsul #\s) (char/= novaConsul #\S))
                        (return)
                    )
                )
            )
            ((and (= opDep 3) (> (length listDepen) 0)) 
                (loop

                    (princ "---------------") (terpri)
                    (princ "   ATUALIZAR   ") (terpri)
                    (princ "---------------") (terpri)

                    (listarDepen listDepen)
                    (setq idEscolhido (read))

                    (when (or (< idEscolhido 0) (>= idEscolhido (length listDepen)))
                        (terpri)(terpri)
                        (princ "ERROR!!! Opção INVÁLIDA!!") (terpri)
                        (princ "ERROR!!! ID não encontrado!!") (terpri)
                        (return)
                    )

                    (detalheDepen idEscolhido listDepen)
                    (princ "Tem certeza que deseja alterar ? (S/N) ")
                    (setq confirm (read-char))

                    (when (or (char= confirm #\s) (char= confirm #\S))
                        (setq dep (nth idEscolhido listDepen))
                        
                        (clear-input)
                        (princ "Nome: ")
                        (setq nome (read-line))
                        (princ "CPF: ")
                        (setq cpf (read-line))
                        (princ "Sexo: ")
                        (setq sexo (read-line))
                        (princ "Tipo: ")
                        (setq tipo (read-line))

                        (setf (Dependente-nome dep) nome)
                        (setf (Dependente-cpf dep) cpf)
                        (setf (Dependente-sexo dep) sexo)
                        (setf (Dependente-tipo dep) tipo)

                        (terpri)
                        (princ "Alterado com SUCESSO!!!")
                        (terpri)
                    )

                    (return)
                )
            )
            ((and (= opDep 4) (> (length listDepen) 0))   
                (loop
                    (princ "---------------") (terpri)
                    (princ "    DELETAR    ") (terpri)
                    (princ "---------------") (terpri)

                    (listarDepen listDepen)
                    (setq idEscolhido (read))

                    (when (or (< idEscolhido 0) (>= idEscolhido (length listDepen)))
                        (terpri)(terpri)
                        (princ "ERROR!!! Opção INVÁLIDA!!") (terpri)
                        (princ "ERROR!!! ID não encontrado!!") (terpri)
                        (return)
                    )

                    (detalheDepen idEscolhido listDepen)
                    (princ "Tem certeza que deseja deletar ? (S/N) ")
                    (setq confirm (read-char))

                    (when (or (char= confirm #\s) (char= confirm #\S))
                        (setq dep (nth idEscolhido listDepen))
                        
                        (if (= idEscolhido 0)
                            (pop listDepen)
                            (delete dep listDepen)
                        )

                        (terpri)
                        (princ "Deletado com SUCESSO!!!")
                        (terpri)
                    )

                    (return)
                )

            )
            ((and (= opDep 5) (> (length listDepen) 0)) 
                (princ "---------------") (terpri)
                (princ "  LISTA TODOS  ") (terpri)
                (princ "---------------") (terpri)

                (princ "=============================")(terpri)
                (dolist (i listDepen)
                    (format t "Nome: ~d ~%" (Dependente-nome i))
                    (format t "CPF: ~d ~%" (Dependente-cpf i))
                    (format t "Sexo: ~d ~%" (Dependente-sexo i))
                    (format t "Tipo: ~d ~%" (Dependente-tipo i))
                    (princ "=============================")(terpri)
                )
                (terpri)
            )
            ((= opDep 6) 
                (princ "--------------") (terpri)
                (princ "     SAIR     ") (terpri)
                (princ "--------------") (terpri)
                (princ "Saindo........") (terpri)
                (return)
            )
            ((or (< opDep 1) (> opDep 6)) 
                (terpri)                    
                (princ "ERROR!!! Opção INVÁLIDA!!") (terpri)
            )
            ((<= (length listDepen) 0)
                (terpri)                    
                (princ "ERROR!!! A lista está vazia!!") (terpri)
                    )
        )

        (setf (Funcionario-listDepen func) listDepen)
    )

)


(defun listarDepen (lista)
    (setq index 0)
    (terpri)
    (princ "-----------------------------")
    (dolist (i lista)
        (format t "~% ID: ~d.... Nome: ~d" index (Dependente-nome i))
        (incf index)
    )
    (terpri)
    (princ "-----------------------------")(terpri)
    (princ "Escolha o ID do dependente: ")
)

(defun detalheDepen (id lista)

    (setq dep (nth id lista))
    (terpri)(terpri)
    (princ "=============================")(terpri)
    (format t "Nome: ~d ~%" (Dependente-nome dep))
    (format t "CPF: ~d ~%" (Dependente-cpf dep))
    (format t "Sexo: ~d ~%" (Dependente-sexo dep))
    (format t "Tipo: ~d ~%" (Dependente-tipo dep))
    (princ "=============================")(terpri)(terpri)

)
