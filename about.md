---
layout: page
title: About this website
---

I'm not sure yet what this website is about, but I'm sure I'll work it out soon.

{% for person in site.data.people %}
 {% if person.status == "present" %}
  {% assign person_id = person.id %}
  {{ person.name }} {{ person.korean_name }} https://github.com/{{ person.github }}
 {% endif %}
{% endfor %}

AAAAAAAAAA BBBBBBBB

{% for person in site.data.people %}
{% if person.status == "present" %}

{% assign person_id = person.id %}
{% capture person_url %}{% include person_url.md person_id=person_id %}{% endcapture %}

- {{ person.name }}
  [:house:]({{ person_url }})
  [:octocat:](https://github.com/{{ person.github }})
  ({{ person.title }})

{% endif %}
{% endfor %}
