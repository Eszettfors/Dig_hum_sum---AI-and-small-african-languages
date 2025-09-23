import requests
from bs4 import BeautifulSoup
import pandas as pd

url = "https://huggingface.co/languages"
resp = requests.get(url)
resp.raise_for_status()

soup = BeautifulSoup(resp.text, "html.parser")

table = soup.find("table")
rows = table.find_all("tr")

data = []
for r in rows[1:]:  # skip header
    cols = r.find_all(["td","th"])
    # Sometimes “th” used for first col
    if len(cols) >= 4:
        # Example columns: Language (name + ISO etc), ISO code, Datasets count, Models count
        lang = cols[0].text.strip()
        iso = cols[1].text.strip()
        datasets = cols[2].text.strip().replace(",", "")
        models = cols[3].text.strip().replace(",", "")
        data.append({
            "language": lang,
            "iso": iso,
            "datasets": int(datasets),
            "models": int(models)
        })

df = pd.DataFrame(data)
print(df.head())


df.to_csv("data/raw/hugging_face_language_data.csv", index = False)