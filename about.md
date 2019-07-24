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
