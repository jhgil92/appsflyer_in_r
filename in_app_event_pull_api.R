# 앱스플라이어 Raw Data 리포트 API 세팅 - In App Event

"""
앱스플라이어 Pull API를 통해 Non-Organic In App Event의 Raw Data를 다운로드한 후,
포털(네이버, 다음) 키워드 광고의 검색어를 parsing하여
키워드 단위의 이벤트 발생건수를 추적하는 코드입니다.
기간을 입력하면(from, to) 매체/키워드별 이벤트 발생건수가 조회됩니다.
Output 데이터는 raw data와 keyword report입니다.

- Appsflyer API Policy : https://support.appsflyer.com/hc/en-us/articles/207034366-API-Policy
- Every API call, that queries data for more than 2 days, is limited to 24 calls per day per application and to 120 calls per day per account
"""

library(dplyr)
library(stringr)
library(urltools)
library(httr)

af_keyword_report <- function(from = '2019-05-26' , to = '2019-06-24', event = '{event_name}'){
  # API Token값 세팅
  token = '{API_TOKEN}' # API token은 각자의 환경에 맞춰서 수정
  
  # API URL 생성
  and_url = paste0("https://hq.appsflyer.com/export/{app id}/in_app_events_report/v5?api_token=", token, "&from=", from, "&to=", to, "&timezone=Asia%2fSeoul&additional_fields=install_app_store,contributor1_match_type,contributor2_match_type,contributor3_match_type,match_type,device_category,gp_referrer,gp_click_time,gp_install_begin,amazon_aid,keyword_match_type&event_name=", event)
  ios_url = paste0("https://hq.appsflyer.com/export/{app id}/in_app_events_report/v5?api_token=", token, "&from=", from, "&to=", to, "&timezone=Asia%2fSeoul&additional_fields=install_app_store,contributor1_match_type,contributor2_match_type,contributor3_match_type,match_type,device_category,gp_referrer,gp_click_time,gp_install_begin,amazon_aid,keyword_match_type&event_name=", event)
  
  # 데이터 조회
  raw_data_and = content(GET(and_url))
  cat('\n', "================android 데이터 다운로드 완료================")
  raw_data_ios = content(GET(ios_url))
  cat('\n', "================ios 다운로드 완료================")
  cat('\n', "================다운로드 완료================")
  
  rbind(raw_data_and, raw_data_ios) %>% as_tibble -> raw_data
  colnames(raw_data) <- gsub(" ", "_", colnames(raw_data))
  
  # 키워드 파싱
  decode_query <- function(strings){
    decode <- function(string){
      string %>% url_decode() -> string
      Encoding(string) <- 'UTF-8'
      return(string)
    }
    return(ifelse(is.na(strings), NA, decode(strings)))
  } # Encoding된 query를 텍스트로 변환하는 함수
  
  raw_data %>%
    mutate(
      query = ifelse(str_detect(.$Original_URL, "n_query"), param_get(.$Original_URL, 'n_query') %>% t,
                     ifelse(str_detect(.$Original_URL, 'DMKW'), param_get(.$Original_URL, "DMKW") %>% t,
                            ifelse(str_detect(.$Original_URL, 'NaPm='), param_get(.$HTTP_Referrer, "query") %>% t, 'NA'))) %>% unlist
    ) %>%
    mutate(query = sapply(query, decode_query) %>% as.vector) ->> raw_data # query에 decode_query 함수 적용 / raw_data는 전역변수로 대입
  
  # 키워드 리포트 생성
  raw_data %>%
    mutate(query = ifelse(is.na(query), "", query))
    group_by(Media_Source, query) %>%
    summarise(KWD_CNT = n()) %>%
    arrange(desc(Media_Source), desc(KWD_CNT)) %>% 
    distinct ->> keyword_report # keyword_report 전역변수로 대입
  
  # 결과값 출력
  cat('\n', from, " ~ ", to, "매체/키워드별 이벤트건수")
  print(keyword_report)
}


af_keyword_report('2019-05-26', '2019-06-24')
