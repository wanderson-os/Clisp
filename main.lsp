(in-package :user)
(compile-file "crud/Funcionario/funcionario.lsp")
(load "crud/Funcionario/funcionario")


(compile-file "crud/Dependente/dependente.lsp")
(load "crud/Dependente/dependente")

(compile-file "crud/Departamento/departamento.lsp")
(load "crud/Departamento/departamento")

(compile-file "crud/Filial/filial.lsp")
(load "crud/Filial/filial")

(defun main ()
    (loop
        (terpri)(terpri)
        (princ "==============") (terpri) 
        (princ "--------------") (terpri) 
        (princ "1.. Funcionário") (terpri) 
        (princ "2.. Filial") (terpri) 
        (princ "3.. Sair") (terpri) 
        (princ "Escolha uma opção acima: ")(terpri)
        (setq opCrud (read))

        (cond
            ((= opCrud 1)
                (mainFuncionario)
            )
            
            ((= opCrud 2)
                (mainFilial)
            )

         
            ((= opCrud 3)
                (princ "--------------") (terpri)
                (princ "     SAIR     ") (terpri)
                (princ "--------------") (terpri)
                (princ "Saindo........") (terpri)
                (return)
            )
            ((or (> opCrud 1) (< opCrud 1)) 
                (terpri)
                (princ "ERROR!!! Opção INVÁLIDA!!") (terpri)
            )
        )
    )

)

(defun selectOption ()
    (terpri)
    (princ "1.. Criar") (terpri)
    (princ "2.. Consultar") (terpri)
    (princ "3.. Atualizar") (terpri)
    (princ "4.. Deletar") (terpri)
    (princ "5.. Listar todos") (terpri)
    (princ "6.. Sair") (terpri)
    (princ "Escolha uma opção acima: ")
)

