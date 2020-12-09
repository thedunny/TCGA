# Pré-Processamento de Dados Clínicos do Consórcio TCGA.

## Descrição

Esse repositório apresenta o arquivos e códigos utilizados para realizar o préprocessamento de dados clínicos relacionados ao subtipo de cancer Adenocarcinoma.

## Desenvolvimento

O préprocessamento foi realizado utilizando a linguagem R, e seu fluxo de trabalho se dividiu entre 4 etapas:

- Seleção de tabelas relacionadas ao subtipo de cancer Adenocarcinoma;
- Remoção de ruídos;
- Substituição de dados ausentes pelo termo NA;
- Cálculo da porcentagem de valores ausentes em cada coluna, e remoção das colunas que tivessem mais de 90% dos dados faltantes e que não representassem mudança na capacidade analítica do conjunto original dos dados;

Os códigos utilizados para a realização dessas etapas estão localizados dentro da pasta scripts no arquivo "script final.R".

## Utilização

Para acessar e utilizar o *data frame* resultante do préprocessamento é só acessar o link abaixo: 

https://drive.google.com/file/d/1I0dlL93kmikhNRsc3C_lkyyc-5EB12PR/view?usp=sharing

O arquivo está em formato .csv e pode ser utilizado em diversas aplicações.

## Referências

Os resultados aqui mostrados são todos baseados em dados disponibilizados pelo *TCGA  Research Network:* [https://www.cancer.gov/tcga](https://www.cancer.gov/tcga).

## Créditos

Danielle Escobar danielleb.escobar@gmail.com
