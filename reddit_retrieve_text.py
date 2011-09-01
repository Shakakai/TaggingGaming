import urllib2, json


URLS = [
"http://www.reddit.com/r/gaming/comments/it7x0/what_left_for_dead_has_taught_me_in_case_there_is/.json",
"http://www.reddit.com/r/gaming/comments/it20m/using_the_stagecoach_as_a_boat_in_red_dead/.json"
]


def fetch(url):
    req = urllib2.urlopen(url)
    txt = req.read()
    return json.loads(txt)

def process_comment(comment):
    txt = ""
    for child in comment['data']['children']:
        txt += "%s\n" % child['data']['body']
        replies = child['data']['replies']
        if replies is not None and replies != '':
            txt += process_comment(replies)
        
    return txt

def create_filename(title):
    filename = title.replace(" ", "_").replace(".", "").replace("?","")
    return filename

def save_text(url):
    data = fetch(url)
    post = data[0]
    filename = create_filename(post["data"]["children"][0]['data']["title"])
    complete_text = "%s \n" % post["data"]["children"][0]['data']["selftext"]
    comment = data[1]
    complete_text += process_comment(comment)
    f = open("./reddit_examples/%s.txt" % filename, "wb")
    f.write(complete_text)
    f.close()

for url in URLS:
    save_text(url)

