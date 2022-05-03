(in-package :user)

(defstruct Departamento
   nome
   descricao
   funcionarios
)

(defun mainDepartamento(idFilial)
    (setq filial (nth idFilial filiais))
    (setq  lDepartamentos (Filial-departamentos filial))

    (loop
        (terpri)
        (princ "---------------") (terpri)
        (princ "  Departamentos  ") (terpri)
        (princ "---------------") (terpri)
        (princ "=== Filial responsável ===") (terpri)
        (format t "Nome : ~d ~%" (Filial-nome filial))
        (terpri)
        (princ "1.. Criar") (terpri)
        (princ "2.. Consultar") (terpri)
        (princ "3.. Atualizar") (terpri)
        (princ "4.. Deletar") (terpri)
        (princ "5.. Listar todos") (terpri)
        (princ "6.. Adicionar funcionário") (terpri)
        (princ "7.. Sair") (terpri)
        (princ "Escolha uma opção acima: ")




        (setq opDep (read))

        (cond
            ((= opDep 1)
                (princ "---------------") (terpri) 
                (princ "     CRIAR     ") (terpri)
                (princ "---------------") (terpri)

                (princ "Nome: ")
                (setq nome (read-line))
                (princ "descricao: ")
                (setq descricao (read-line))
             

                ( setq dep (make-Departamento
                    :nome nome
                    :descricao descricao
                    :funcionarios (list )
                )
                )
                (if ( = 0 (length lDepartamentos))
                    (push dep lDepartamentos)
                    (push dep (cdr (last lDepartamentos)))
                )
            )
            ((and (= opDep 2) (> (length lDepartamentos) 0)) 

                (loop

                    (princ "---------------") (terpri)
                    (princ "   CONSULTAR   ") (terpri)
                    (princ "---------------") (terpri)

                    (listarDepartamentos lDepartamentos)
                    (setq idEscolhido (read))

                    (when (or (< idEscolhido 0) (>= idEscolhido (length lDepartamentos)))
                        (terpri)(terpri)
                        (princ "ERROR!!! Opção INVÁLIDA!!") (terpri)
                        (princ "ERROR!!! ID não encontrado!!") (terpri)
                        (return)
                    )

                    (detalheDepen idEscolhido lDepartamentos)
                    (princ "Deseja fazer outra consulta? (S/N) ")
                    (setq novaConsul (read-char))

                    (when (and (char/= novaConsul #\s) (char/= novaConsul #\S))
                        (return)
                    )
                )
            )
            ((and (= opDep 3) (> (length lDepartamentos) 0)) 
                (loop

                    (princ "---------------") (terpri)
                    (princ "   ATUALIZAR   ") (terpri)
                    (princ "---------------") (terpri)

                    (listarlDepartamentos lDepartamentos)
                    (setq idEscolhido (read))

                    (when (or (< idEscolhido 0) (>= idEscolhido (length lDepartamentos)))
                        (terpri)(terpri)
                        (princ "ERROR!!! Opção INVÁLIDA!!") (terpri)
                        (princ "ERROR!!! ID não encontrado!!") (terpri)
                        (return)
                    )

                    (detalheDepen idEscolhido lDepartamentos)
                    (princ "Tem certeza que deseja alterar ? (S/N) ")
                    (setq confirm (read-char))

                    (when (or (char= confirm #\s) (char= confirm #\S))
                        (setq dep (nth idEscolhido lDepartamentos))
                        (clear-input)
                        (princ "Nome: ")
                        (setq nome (read-line))
                        (princ "Descrição: ")
                        (setq descricao (read-line))

                        (setf (Departamento-nome dep) nome)
                        (setf (Departamento-descricao dep) descricao)
                    

                        (terpri)
                        (princ "Alterado com SUCESSO!!!")
                        (terpri)
                    )

                    (return)
                )
            )
            ((and (= opDep 4) (> (length lDepartamentos) 0))   
                (loop
                    (princ "---------------") (terpri)
                    (princ "    DELETAR    ") (terpri)
                    (princ "---------------") (terpri)

                    (listarlDepartamentos lDepartamentos)
                    (setq idEscolhido (read))

                    (when (or (< idEscolhido 0) (>= idEscolhido (length lDepartamentos)))
                        (terpri)(terpri)
                        (princ "ERROR!!! Opção INVÁLIDA!!") (terpri)
                        (princ "ERROR!!! ID não encontrado!!") (terpri)
                        (return)
                    )

                    (detalheDepen idEscolhido lDepartamentos)
                    (princ "Tem certeza que deseja deletar ? (S/N) ")
                    (setq confirm (read-char))

                    (when (or (char= confirm #\s) (char= confirm #\S))
                        (setq dep (nth idEscolhido lDepartamentos))
                        
                        (if (= idEscolhido 0)
                            (pop lDepartamentos)
                            (delete dep lDepartamentos)
                        )

                        (terpri)
                        (princ "Deletado com SUCESSO!!!")
                        (terpri)
                    )

                    (return)
                )

            )
            ((and (= opDep 5) (> (length lDepartamentos) 0)) 
                (princ "---------------") (terpri)
                (princ "  LISTA TODOS  ") (terpri)
                (princ "---------------") (terpri)

                (princ "=============================")(terpri)
                (dolist (i lDepartamentos)
                    (format t "Nome: ~d ~%" (Departamento-nome i))
                    (format t "Descriçao: ~d ~%" (Departamento-descricao i))
                    (format t "Quantidade de funcionários ~d ~%"(length (Departamento-funcionarios i)))
                    (terpri)

                    (if (>=(length(Departamento-funcionarios i))1)
                    
                    (funcionariosDepartamentos i)

                    )
                    (princ "=============================")(terpri)
                )
                (terpri)
            )

            ((= opdep 6)

(if (<= (length lDepartamentos) 0)
                (princ "ERRO!!! Não há registros de departamentos cadastrados para essa filial!!")
(testDep)
)




            )
            ((= opDep 7) 
                (princ "--------------") (terpri)
                (princ "     SAIR     ") (terpri)
                (princ "--------------") (terpri)
                (princ "Saindo........") (terpri)
                (return)
            )
            ((or (< opDep 1) (> opDep 7)) 
                (terpri)                    
                (princ "ERROR!!! Opção INVÁLIDA!!") (terpri)
            )
            ((<= (length lDepartamentos) 0)
                (terpri)                    
                (princ "ERROR!!! A lista está vazia!!") (terpri)
                    )
        )

        (setf (Filial-departamentos filial) lDepartamentos)
    )


)


(defun listarDepartamentos (lista)
    (setq index 0)
    (terpri)
    (princ "-----------------------------")
    (dolist (i lista)
        (format t "~% ID: ~d.... Nome: ~d" index (Departamento-nome i))
        (incf index)
    )
    (terpri)
    (princ "-----------------------------")(terpri)
    (princ "Escolha o ID do departamento: ")
)

(defun detalheDepen (id lista)
    (setq dep (nth id lista))
    (terpri)(terpri)
    (princ "=============================")(terpri)
    (format t "Nome: ~d ~%" (Departamento-nome dep))
    (format t "Descrição: ~d ~%" (Departamento-descricao dep))
    (format t "Quantidade de funcionários ~d ~%"(length (Departamento-funcionarios dep)))
    (terpri)

    (setq funs (Departamento-funcionarios dep))

   (dolist (i funs)

    (format t "Funcionário : ~d ~%" (Funcionario-nome i))
    (terpri)

    )
    (princ "=============================")(terpri)(terpri)


    (terpri)


)

(defun testDep ()
        (listarDepartamentos lDepartamentos)
        (setq idDep (read))
   (if (<= (length listFunc) 0)
        (princ "ERRO!!! Não há registros de funcionários !!")
        (adicionarFuncionario idDep)
   )
)

(defun adicionarFuncionario (idDepa)
    
    (setq dep (nth idDepa lDepartamentos))
    (setq funcionarios (Departamento-funcionarios dep))

    (listarFunc) 
    (setq idFunc (read)) 

    (if ( = 0 (length funcionarios))
                    (push func funcionarios)
                    (push func (cdr (last funcionarios)))
    )

        (setf (Departamento-funcionarios dep) funcionarios)
        (terpri)
        (princ "ADICIONADO COM SUCESSO!!!")

)

(defun funcionariosDepartamentos(dep)
        (setq funs (Departamento-funcionarios dep))

      (dolist (f funs)
        (format t "Funcionário : ~d ~%" (Funcionario-nome f))
        (terpri)
      )
)
