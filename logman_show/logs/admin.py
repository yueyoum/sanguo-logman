from django.contrib import admin

from logs.models import LogSystem, LogResource

from errormsg_zh import ERRORMSGZH

class LogSystemAdmin(admin.ModelAdmin):
    list_display = (
        'id', 'levelname', 'char_id', 'error_id', 'Msg', 'func_name', 'msg', 'occurred_at',
    )

    list_filter = ('error_id',)

    def Msg(self, obj):
        return ERRORMSGZH[obj.error_id]


class LogResourceAdmin(admin.ModelAdmin):
    list_display = (
        'id', 'char_id', 'income', 'func_name',
        'exp', 'official_exp', 'gold', 'sycee',
        'heros', 'souls',
        'equipments', 'gems', 'stuffs',
        'des',
        'occurred_at'
    )

    list_filter = ('char_id',)


admin.site.register(LogSystem, LogSystemAdmin)
admin.site.register(LogResource, LogResourceAdmin)
