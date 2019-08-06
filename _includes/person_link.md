{%- assign person_id = include.person_id %}
{%- assign person = site.data.people | where:"id",person_id | sample %}
{%- capture url %}{% include person_url.md person_id=person_id %}{%- endcapture %}
[{{ person.name }}]({{ url }})
