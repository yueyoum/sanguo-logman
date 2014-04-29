from django.contrib import admin

from logs.models import Log

class LogAdmin(admin.ModelAdmin):
    list_display = (
        'id', 'levelname', 'node_id', 'error_id', 'char_id', 'func_name', 'msg', 'occurred_at',
    )

    list_filter = ('error_id',)


admin.site.register(Log, LogAdmin)

