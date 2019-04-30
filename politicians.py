import sqlite3
import re
import csv


sqlite_file = '/Users/michaeltauberg/projects/NewsScraper/newsdb_v6.sqlite'
conn = sqlite3.connect(sqlite_file)
c = conn.cursor()

news_rows = []
# shultz, warren, harris, gabbard, booker, sanders, biden

def get_rows(news_rows, last_name, first_name):
	c.execute('SELECT * FROM {tn} WHERE lower({cn}) like "%{sn}%"'\
	        .format(tn="Headlines", cn="headline", sn=last_name))
	all_rows = c.fetchall()

	for row in all_rows:
		date = str(row[1])
		row = row  + (last_name,)
		if (re.match("2019", date) is None):
			continue
		#if (re.match(first_name, row[7].lower()) is None): # double check that an article about "warren" has "elizabeth" in it and is really about her
		#	continue
		news_rows.append(row)
		print row[8]


get_rows(news_rows, "harris", "kamala")
get_rows(news_rows, "warren", "elizabeth")
get_rows(news_rows, "sanders", "bernie")
get_rows(news_rows, "yang", "andrew")  #Sherrod
get_rows(news_rows, "biden", "joe")
#get_rows(news_rows, "rourke", "beto")
get_rows(news_rows, "beto", "beto")
get_rows(news_rows, "booker", "cory")
get_rows(news_rows, "gillibrand", "kirsten")
get_rows(news_rows, "klobuchar", "amy")
get_rows(news_rows, "castro", "julian")
get_rows(news_rows, "gabbard", "tulsi")
get_rows(news_rows, "yang", "andrew")
get_rows(news_rows, "buttigieg", "pete")
get_rows(news_rows, "ocasio", "octavia")
get_rows(news_rows, "trump", "donald")


i = 0
for row in news_rows:
	url = row[3] 
	#if (isinstance(row[4], str)):
	headline = row[4]
	story = row[5]
	date = row[1]
	#print date
	#print headline
	#print url

	i = i + 1
	print i

OUTFILE = "politicians7.csv"

i = 0

with open(OUTFILE, 'w') as output_file:
	keys = ['date', 'authors', 'url', 'headline', 'subject', 'content']
	dict_writer = csv.DictWriter(output_file, keys)
	dict_writer.writeheader()

	for row in news_rows:
		r = {}
		r['date'] = str(row[1]).encode('utf8')
		r['authors'] = row[2].encode('utf8')
		r['url'] = row[3].encode('utf8')
		r['headline'] = row[4].encode('utf8')
		r['content'] = row[7].encode('utf8')
		r['subject'] = row[8].encode('utf8')
		#try:
		
		dict_writer.writerow(r)
		#	print i
		#except:
		#	print("couldn't write row data")

print i
	

# collexct all stories and output a csv