import botometer
import codecs, csv

rapidapi_key = "75b0e3dc5emsh20d577dc3229d14p198c32jsn5ef87f3e68d1"
twitter_app_auth = {
    'consumer_key': '',
    'consumer_secret': '',
    'access_token': '',
    'access_token_secret': '',
  }
bom = botometer.Botometer(wait_on_ratelimit=True,
                          rapidapi_key=rapidapi_key,
                          **twitter_app_auth)

# Check a single account by screen name
#result = bom.check_account('@clayadavis')

# Check a single account by id
#result = bom.check_account(15026691)

# Check a sequence of accounts, separate by comma (',') and with quotation marks ('087393836','7294872398')

accounts = []
ct=0

out=[]
ks=[]
for screen_name, result in bom.check_accounts_in(accounts):
    print(screen_name, result)
    ct+=1
    if ct % 100 == True:

        #it's actually July2022, not Feb 2022
        with codecs.open('name.csv','w', encoding='utf-8') as f:
            w=csv.DictWriter(f, delimiter=';', fieldnames=ks)
            w.writeheader()
            w.writerows(out)
    try:
        print("%s/%s"% (ct, len(accounts)))
        dic = {
        'screen_name': screen_name
        
        }
        for xx in result.keys():
            for kk,vv in result[xx].items():
                if type(vv) == dict:
                    for kkk,vvv in vv.items():
                        dic["%s.%s.%s" % (xx, kk, kkk)] = vvv
                else:
                    dic["%s.%s" % (xx, kk)] = vv
        ks=list(dic.keys())
        out.append(dic)
    except Exception as err:
        print(err)
with codecs.open('name.csv','w', encoding='utf-8') as f:
    w=csv.DictWriter(f, delimiter=';', fieldnames=ks)
    w.writeheader()
    w.writerows(out)