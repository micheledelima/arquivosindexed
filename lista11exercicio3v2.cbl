      $set sourceformat"free"
      *>divisão de identificação do programa
       identification division.
       program-id. "lista11exercicio3v2".
       author. "Michele de Lima".
       installation. "PC".
       date-written. 27/07/2020.
       date-compiled. 28/07/2020.



      *>divisão para configuração do ambiente
       environment division.
       configuration section.
           special-names. decimal-point is comma.

      *>-----Declaração dos recursos externos
       input-output section.
       file-control.
      *>declaração do arquivo
           select arqCadAlu assign to "arqCadAlu.dat" *>Nome do arquivo
           organization is indexed                    *>A forma de organização dos dados
           access mode is dynamic                     *>A forma de acesso aos dados
           lock mode is automatic                     *>Tratamento de dead lock - Evita perda de dados
           record key is fd-aluno                     *>Chave relativa para acesso
           file status is ws-fs-arqCadAlu.            *>File status - o status da ultima operação - tratada com msn-erro

       i-o-control.

      *>declaração de variáveis
       data division.

      *>----variaveis de arquivos
       file section.
       fd arqCadAlu.

       01  fd-alunos.
           05  fd-aluno                            pic x(25).
           05  fd-endereco                         pic x(35).
           05  fd-mae                              pic x(25).
           05  fd-pai                              pic x(25).
           05  fd-telefone                         pic x(15).
           05  fd-notas.
               10  fd-nota1                        pic 9(02)v99.
               10  fd-nota2                        pic 9(02)v99.
               10  fd-nota3                        pic 9(02)v99.
               10  fd-nota4                        pic 9(02)v99.
               10  fd-informou                     pic 9(02)
                                                   value zeros.

      *>----variaveis de trabalho
       working-storage section.

       77 ws-fs-arqCadAlu                          pic 9(02).

       01 ws-msn-erro.
          05 ws-msn-erro-ofsset                    pic 9(04).
          05 filler                                pic x(01) value "-".
          05 ws-msn-erro-cod                       pic 9(02).
          05 filler                                pic x(01) value space.
          05 ws-msn-erro-text                      pic x(42).



       01  ws-alunos.
           05  ws-aluno                            pic x(25).
           05  ws-endereco                         pic x(35).
           05  ws-mae                              pic x(25).
           05  ws-pai                              pic x(25).
           05  ws-telefone                         pic x(15).
           05  ws-notas.
               10  ws-nota1                        pic 9(02)v99
                                                   value zeros.
               10  ws-nota2                        pic 9(02)v99
                                                   value zeros.
               10  ws-nota3                        pic 9(02)v99
                                                   value zeros.
               10  ws-nota4                        pic 9(02)v99
                                                   value zeros.
               10  ws-informou                     pic 9(02)
                                                   value zeros.
               10 ws-informoun                     pic x(02)
                                                   value zeros.

       77 ws-sair                                  pic x(01).
          88  fechar-programa                      value "N" "n".
          88  voltar-tela                          value "V" "v".

       77  ws-informa                              pic x(02).
       77  ws-informa1                             pic x(02).
       77  ws-menu                                 pic x(02).


      *>----variaveis para comunicação entre programas
       linkage section.


      *>----declaração de tela
       screen section.


      *>declaração do corpo do programa
       procedure division.


           perform inicializa.
           perform processamento.
           perform finaliza.

      *>------------------------------------------------------------------------
      *>  procedimentos de inicialização
      *>------------------------------------------------------------------------
       inicializa section.
      *>   abre o arquivo para leitura e escrita

           open i-o arqCadAlu
           if  ws-fs-arqCadAlu <> 00
           and ws-fs-arqCadAlu <> 05 then
               move 1                                to ws-msn-erro-ofsset
               move ws-fs-arqCadAlu                  to ws-msn-erro-cod
               move "Erro ao abrir arq. arqCadAlu "  to ws-msn-erro-text
               perform finaliza-anormal
           end-if
           .
       inicializa-exit.
           exit.

      *>------------------------------------------------------------------------
      *>  Processamento Principal
      *>------------------------------------------------------------------------
       processamento section.

           perform until fechar-programa
               move space to ws-sair

               display "'CA' Cadastrar Alunos?"
               display "'CN' Cadastrar Notas?"
               display "'CO' Consulta Indexada de Alunos?"
               display "'DA' Deletar Cadastro de Alunos? "
               display "'AC' Alterar Cadastro? "
               display "'CS' Consulta Sequencial?"
               accept ws-menu

                   evaluate ws-menu
                       when = "CA"
                           perform cadastrar-aluno
                       when = "CN"
                           perform cadastrar-notas
                       when = "CO"
                           perform consultar-cadastro
                       when = "DA"
                           perform deletar-aluno
                       when = "AC"
                           perform alterar-cadastro
                       when = "CS"
                           perform consulta-seq

                       when other
                           display "opcao invalida"
                   end-evaluate


           end-perform
           .
       processamento-exit.
           exit.

      *>------------------------------------------------------------------------
      *>  cadastro de alunos
      *>------------------------------------------------------------------------
       cadastrar-aluno section.


           perform until voltar-tela
           display erase


               move ws-alunos to fd-alunos


               display "-------  cadastro de alunos -------"

               display "Nome Aluno: "
               accept ws-aluno
               display "Endereco: "
               accept ws-endereco
               display "Nome da mae: "
               accept ws-mae
               display "Nome do pai: "
               accept ws-pai
               display "Telefone: "
               accept ws-telefone


      *>       salva os dados informados no arquivo
                write fd-alunos from ws-alunos
                if ws-fs-arqCadAlu <> 0 then
                    move 2                                   to ws-msn-erro-ofsset
                    move ws-fs-arqCadAlu                     to ws-msn-erro-cod
                    move "Erro ao escrever arq. arqCadAlu "  to ws-msn-erro-text
                    perform finaliza-anormal
                end-if



               display "Deseja cadastrar mais um Aluno? 'S' ou 'V'oltar"
               accept ws-sair

           end-perform
           .
       cadastrar-aluno-exit.
           exit.


      *>------------------------------------------------------------------------
      *>  cadastro de notas do aluno
      *>------------------------------------------------------------------------
       cadastrar-notas section.

           perform until voltar-tela

               move ws-alunos to fd-alunos


               display "------ Cadastro de notas ------"

               display "Informe o aluno: "
               accept ws-aluno

                   display "Informe a nota1: "
                   accept ws-nota1

                   display "Informe a nota2: "
                   accept ws-nota2

                   display "Informe a nota3: "
                   accept ws-nota3

                   display "Informe a nota4: "
                   accept ws-nota4

                   move 1 to ws-informou



      *>       salva os dados informados no arquivo
                   rewrite fd-alunos from ws-alunos
                   if ws-fs-arqCadAlu <> 0 then
                       move 3                                   to ws-msn-erro-ofsset
                       move ws-fs-arqCadAlu                     to ws-msn-erro-cod
                       move "Erro ao escrever arq. arqCadAlu "  to ws-msn-erro-text
                       perform finaliza-anormal
                   end-if



               display "Deseja cadastrar mais uma nota para outro aluno? 'S' ou 'V'oltar"
               accept ws-sair

           end-perform

           .
       cadastrar-notas-exit.
           exit.

      *>------------------------------------------------------------------------
      *>Consulta o cadastro do aluno
      *>------------------------------------------------------------------------
       consultar-cadastro section.

               display "Informe o aluno a ser consultado: "
               accept ws-aluno

                   move ws-aluno to fd-aluno
                   read arqCadAlu
                   if ws-fs-arqCadAlu  <> 0
                   and ws-fs-arqCadAlu <> 10 then
                       if ws-fs-arqCadAlu = 23 then
                           display "Aluno informado não encontrado"
                       else
                           move 4                                to ws-msn-erro-ofsset
                           move ws-fs-arqCadAlu                  to ws-msn-erro-cod
                           move "Erro ao ler arq. arqCadAlu "    to ws-msn-erro-text
                           perform finaliza-anormal
                       end-if
                   end-if


                       move  fd-endereco       to ws-endereco
                       move  fd-mae            to ws-mae
                       move  fd-pai            to ws-pai
                       move  fd-telefone       to ws-telefone
                       move  fd-nota1          to ws-nota1
                       move  fd-nota2          to ws-nota2
                       move  fd-nota3          to ws-nota3
                       move  fd-nota4          to ws-nota4
                       move  fd-informou       to ws-informou

               *> -------------

                       display "Nome Aluno : " ws-aluno
                       display "Endereco   : " ws-endereco
                       display "Nome da mae: " ws-mae
                       display "Nome do pai: " ws-pai
                       display "Telefone   : " ws-telefone
                       *>tratamento caso o cadastro do aluno não tenha notas
                       if ws-informou = 0 then
                          display "Não possui notas informadas"

                          display "Deseja informar notas 'S'im ou 'N'ao?"
                          accept ws-informoun

                           if ws-informoun = "S"  then
                               perform cadastrar-notas
                           end-if
                       else
                           display "Nota 1     : " ws-nota1
                           display "Nota 2     : " ws-nota2
                           display "Nota 3     : " ws-nota3
                           display "Nota 4     : " ws-nota4
                      end-if


           .
       consultar-cadastro-exit.
           exit.



      *>------------------------------------------------------------------------
      *>Deleta o cadastro de determinado aluno
      *>------------------------------------------------------------------------
       deletar-aluno section.

           perform until voltar-tela


           display "Informe o aluno que deseja excluir: "
           accept ws-aluno

               move ws-aluno to fd-aluno
               delete arqCadAlu
               if ws-fs-arqCadAlu = 0 then
                   display "Aluno " ws-aluno " apagado com sucesso!"
               else
                   if ws-fs-arqCadAlu = 23 then
                       display "Aluno informado é invalido"
                   else
                       move 5                                     to ws-msn-erro-ofsset
                       move ws-fs-arqCadAlu                       to ws-msn-erro-cod
                       move "Erro ao ler arq. arqCadAlu "         to ws-msn-erro-text
                       perform finaliza-anormal
               end-if
                   end-if

               display "Deseja deletar mais um aluno? 'S' ou 'V'oltar"
               accept ws-sair

           end-perform

           .
       deletar-aluno-exit.
           exit.



      *>------------------------------------------------------------------------------------------------------
      *>Alteração no cadastro do aluno
      *>------------------------------------------------------------------------------------------------------
       alterar-cadastro section.


               perform consultar-cadastro

               display "Deseja alterar as notas 'N'? "
               display "Deseja alterar cadastro do aluno 'C'? "
               accept ws-informa

      *>-----------------Altera as notas----------------------------------------------------------------------
               if ws-informa = "N" then

                   display "Informe a nova nota1: "
                   accept ws-nota1
                   display "Informe a nova nota2: "
                   accept ws-nota2
                   display "Informe a nova nota3: "
                   accept ws-nota3
                   display "Informe a nova nota4: "
                   accept ws-nota4

                   move ws-nota1 to fd-nota1
                   move ws-nota2 to fd-nota2
                   move ws-nota3 to fd-nota3
                   move ws-nota4 to fd-nota4
                   rewrite fd-alunos
                       if ws-fs-arqCadAlu = 0 then
                           display "Notas alteradas com sucesso"
                       else
                           move 6                                   to ws-msn-erro-ofsset
                           move ws-fs-arqCadAlu                     to ws-msn-erro-cod
                           move "Erro ao alterar arq. CadAluno "    to ws-msn-erro-text
                           perform finaliza-anormal

                       end-if
               end-if


      *>----------------Altera o cadastro do aluno---------------------------------------------------------------

               if ws-informa = "C"

                   display "Deseja alterar o endereço 'EN', telefone 'TE, nome dos pais 'PS'? "
                   accept ws-informa1

      *>------------------Altera o endereço------------------------------------------------------------------------
                       if ws-informa1 = "EN" then

                           display "Informe o novo endereço: "
                           accept ws-endereco

                           move ws-endereco to fd-endereco
                           rewrite fd-alunos
                               if ws-fs-arqCadAlu = 0 then
                                   display "Endereco alterado com sucesso"
                               else
                                   move 7                                   to ws-msn-erro-ofsset
                                   move ws-fs-arqCadAlu                     to ws-msn-erro-cod
                                   move "Erro ao alterar arq. arqCadAlu "   to ws-msn-erro-text
                                   perform finaliza-anormal
                               end-if
                       end-if

      *>-------------------Altera o telefone------------------------------------------------------------------------
                       if ws-informa1 = "TE" then

                           display "Informe o novo telefone: "
                           accept ws-telefone

                           move ws-telefone to fd-telefone
                           rewrite fd-alunos
                               if ws-fs-arqCadAlu = 0 then
                                   display "Telefone alterado com sucesso"
                               else
                                   move 8                                   to ws-msn-erro-ofsset
                                   move ws-fs-arqCadAlu                     to ws-msn-erro-cod
                                   move "Erro ao alterar arq. arqCadAlu "   to ws-msn-erro-text
                                   perform finaliza-anormal

                               end-if
                       end-if

      *>-------------------Altera o nome dos pais---------------------------------------------------------------------
                       if ws-informa1 = "PS" then

                           display "Informe o novo nome da mãe: "
                           accept ws-mae
                           display "Informe o novo nome do pai: "
                           accept ws-pai

                           move ws-mae to fd-mae
                           move ws-pai to fd-pai
                           rewrite fd-alunos
                               if ws-fs-arqCadAlu = 0 then
                                   display "Nomes dos pais alterados com sucesso"
                               else
                                   move 9                                   to ws-msn-erro-ofsset
                                   move ws-fs-arqCadAlu                     to ws-msn-erro-cod
                                   move "Erro ao alterar arq. arqCadAlu "   to ws-msn-erro-text
                                   perform finaliza-anormal

                               end-if
                       end-if
               end-if


           .
       alterar-cadastro-exit.
           exit.



      *>------------------------------------------------------------------------------------------------------
      *>Consulta de aluno sequencial - le o arquivo de forma sequencial
      *>------------------------------------------------------------------------------------------------------
       consulta-seq section.


               read arqCadAlu next
               if ws-fs-arqCadAlu <> 0 then
                   if ws-fs-arqCadAlu = 10 then
                       perform consulta-seq-prev
                   else
                      move 10                                  to ws-msn-erro-ofsset
                      move ws-fs-arqCadAlu                     to ws-msn-erro-cod
                      move "Erro ao ler arq. arqCadAlu "       to ws-msn-erro-text
                      perform finaliza-anormal
                  end-if
               end-if

               move fd-alunos to ws-alunos

      *> -------------
               display "Aluno: "  ws-aluno
               display "'Enter' para prosseguir com os nomes dos prox. alunos"



           .
       consulta-seq-exit.
           exit.




      *>------------------------------------------------------------------------------------------------------
      *>Consulta de aluno sequencial - le o arquivo de forma sequencial
      *>------------------------------------------------------------------------------------------------------
       consulta-seq-prev section.


               read arqCadAlu previous
               if ws-fs-arqCadAlu <> 0 then
                   if ws-fs-arqCadAlu = 10 then
                       perform consulta-seq
                   else
                      move 11                                    to ws-msn-erro-ofsset
                      move ws-fs-arqCadAlu                       to ws-msn-erro-cod
                      move "Erro ao ler arq. arqCadAlu "         to ws-msn-erro-text
                      perform finaliza-anormal
                  end-if
               end-if

               move fd-alunos to ws-alunos

      *> -------------
               display "Aluno: "  ws-aluno
               display "'Enter' para prosseguir com os nomes dos prox. alunos"




           .
       consulta-seq-prev-exit.
           exit.



      *>------------------------------------------------------------------------------------------------------
      *>Finalização do sistema Anormal
      *>------------------------------------------------------------------------------------------------------
       finaliza-anormal section.
           display erase
           display ws-msn-erro.
           Stop run
           .
       finaliza-anormal-exit.
           exit.



      *>------------------------------------------------------------------------------------------------------
      *>Finalização do sistema
      *>------------------------------------------------------------------------------------------------------
       finaliza section.

           close arqCadAlu
           if ws-fs-arqCadAlu <> 0 then
               move 12                                 to ws-msn-erro-ofsset
               move ws-fs-arqCadAlu                    to ws-msn-erro-cod
               move "Erro ao fechar arq. arqCadAlu "   to ws-msn-erro-text
               perform finaliza-anormal
           end-if


           Stop run
           .
       finaliza-exit.
           exit.




