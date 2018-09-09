import scrapy


class MetaCriticSpider(scrapy.Spider):
    name = "Metacritic"
    start_urls = [
        'http://www.metacritic.com/browse/games/score/metascore/all/pc/filtered?view=detailed',
    ]

    def parse_details(self, response):

        AmountCritics = response.css('div.metascore_wrap.highlight_metascore div.summary p span.count a span::text').extract_first()
        if AmountCritics is not None:
            AmountCritics = AmountCritics.strip()

        AmountUsers = response.css('div.userscore_wrap.feature_userscore div.summary p span.count a::text').extract_first()
        if AmountUsers is not None:
            AmountUsers = AmountUsers.split(' ')[0]

        Developer = response.css('li.summary_detail.developer span.data::text').extract_first()
        if Developer is not None:
            Developer = Developer.strip()
        
        yield {
            'Title': response.css('div.product_title a.hover_none span h1::text').extract_first(),
            'Release Date': response.css('ul.summary_details li.summary_detail.release_data span.data::text').extract_first(),
            'Meta Score': response.css('div.metascore_w.xlarge.game span::text').extract_first(),
            'User Score': response.css('div.metascore_w.user.large.game::text').extract_first(),
            'AmountCritics': AmountCritics,
            'AmountUsers': AmountUsers,
            'Developer': Developer
        }

    def parse(self, response):

        mainbody = response.css('div#main')

        for game in mainbody.css('div.product_basics.stats'):
            # yield {
            #     'Title': game.css('h3.product_title a::text').extract_first(),
            #     'Release Date': game.css('li.release_date span.data::text').extract_first(),
            #     'Meta Score': game.css('span.metascore_w::text').extract_first(),
            #     'User Score': game.css('li.product_avguserscore span.data::text').extract_first(),
            # }
            details = game.css('h3.product_title a::attr("href")').extract_first()
            if details is not None:
                yield response.follow(details, self.parse_details)

        next_page = response.css('span.flipper.next a::attr("href")').extract_first()
        if next_page is not None:
            yield response.follow(next_page, self.parse)
