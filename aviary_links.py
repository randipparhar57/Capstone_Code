from playwright.sync_api import sync_playwright
from bs4 import BeautifulSoup
from pandas import pandas
import time

base_url = 'https://queenslibrary.aviaryplatform.com/catalog?current_flock=false&f[access_ss][]=access_public&f[collection_id_is][]=21&f[has_transcript_ss][]=Yes&myresources=0&page='
end_url = '&request_is_xhr=false&sort=title_ss+asc&user_ip=2600:4040:917a:d700:984a:9c88:b69a:ea56&view=list'
pg_num = 1

links = []
page_number = []
while pg_num <= 11:
    with sync_playwright() as p:
        browser = p.chromium.launch()
        context = browser.new_context(
            user_agent = "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/133.0.0.0 Safari/537.36"
        )

        page = context.new_page()
        page.goto(base_url + str(pg_num) + end_url)
        #page.screenshot(path = "screenshot_pg" + str(pg_num) + ".jpg", full_page = True)
        
        content = page.content()
        soup = BeautifulSoup(content, features="html.parser")

        listings = soup.find_all("a", class_="mb-0px")
        for i in listings:
            link = i.get('href')
            links.append(link)

        page_number.append(pg_num)
        browser.close()

    pg_num += 1
    time.sleep(3)

print("Page numbers visited:", page_number)
print("Number of links: ", len(links))


meta_links = []
for i in links:
    with sync_playwright() as p:
        browser = p.chromium.launch()
        context = browser.new_context(
            user_agent="Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) "
                    "AppleWebKit/537.36 (KHTML, like Gecko) "
                    "Chrome/133.0.0.0 Safari/537.36"
        )
        
        page = context.new_page()
        print(f"Scraping link: {i}")
        page.goto(i)
        content = page.content()
        soup = BeautifulSoup(content, features="html.parser")

        meta_div = soup.find('div', class_='info_tabs')
        meta_base_url = meta_div.get("data-template-url")
        print("Meta Base URL: ", meta_base_url)
        
        meta_url = "https://queenslibrary.aviaryplatform.com" + meta_base_url
        print("Meta URL: ", meta_url)
        
        meta_links.append(meta_url)
        
        browser.close()
    time.sleep(3)

print("Number of meta urls: ", len(meta_links))


locations = []
interviewees = []
for i in meta_links:
    with sync_playwright() as p:
        browser = p.chromium.launch()
        context = browser.new_context(
            user_agent="Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) "
                    "AppleWebKit/537.36 (KHTML, like Gecko) "
                    "Chrome/133.0.0.0 Safari/537.36"
        )
        
        page = context.new_page()
        print(f"Scraping link: {i}")
        page.goto(i)

        content = page.content()
        soup = BeautifulSoup(content, features="html.parser")

        location_span = soup.find('span', class_='badge badge-secondary single_value_non_tombstone', string=lambda text: text and text.lower() == 'spatial')
        if location_span:
            location_value = location_span.find_next_sibling('span', class_='single_value single_value_non_tombstone')
            if location_value:
                location = location_value.get_text(strip=True)
                print(location) 
            else:
                print("Associated value span not found.")
                location = "QMP Podcast"
        else:
            print("Spatial label not found.")
            location = "QMP Podcast"
        locations.append(location)

        interviewee_span = soup.find('span', class_='badge badge-secondary single_value_non_tombstone', string=lambda text: text and text.lower() == 'interviewee')
        if interviewee_span:
            interviewee_value = interviewee_span.find_next_sibling('span', class_='single_value single_value_non_tombstone')
            if interviewee_value:
                interviewee = interviewee_value.get_text(strip=True)
                print(interviewee) 
                interviewees.append(interviewee)
            else:
                print("Associated value span not found.")
        else:
            print("Interviewee label not found.")
        
        browser.close()
    time.sleep(3)

print("Number of locations: ", len(locations))
print("Number of interviewees: ", len(interviewees))


dict = {'interviewee': interviewees, 'location': locations, 'url': links, 'meta_url': meta_links}
df = pandas.DataFrame(dict)
jamaica_df = df[df['location'].str.contains('Jamaica', case=False, na=False)]
jamaica_df.to_csv('QMP_Jamaica_Interviews.csv', index=False)