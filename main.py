import requests
from bs4 import BeautifulSoup

pinned_repositories =[]

username = input("Enter github username : ")
r = requests.get("https://github.com/" + username)
soup = BeautifulSoup(r.text, 'html.parser')

desired_tags = soup.findAll("li", {"class": "pinned-repo-item"})

for tag in desired_tags:
    repo_data = {}
    repo_data["title"] = (tag.find("span", {"class": "repo"})).text

    div_for_link = (tag.find("span", {"class": "d-block"}))
    repo_data["link"] = (div_for_link.find("a", {"class": "text-bold"}))["href"]
    repo_data["link"] = "https://github.com" + repo_data["link"]

    repo_data["desc"] = (tag.find("p", {"class": "pinned-repo-desc"})).text
    repo_data["desc"] = repo_data["desc"].strip()

    pinned_repositories.append(repo_data)

print(pinned_repositories)
