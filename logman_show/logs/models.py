from django.db import models

class LogSystem(models.Model):
    levelname = models.CharField(max_length=16)
    error_id = models.IntegerField(db_index=True)
    char_id = models.IntegerField()
    func_name = models.CharField(max_length=64)
    msg = models.TextField()
    occurred_at = models.DateTimeField()

    class Meta:
        db_table = 'log_system'


class LogResource(models.Model):
    char_id = models.IntegerField(db_index=True)
    income = models.BooleanField()
    func_name = models.CharField(max_length=64)
    exp = models.IntegerField(default=0)
    official_exp = models.IntegerField(default=0)
    gold = models.IntegerField(default=0)
    sycee = models.IntegerField(default=0)
    heros = models.CharField(max_length=255, blank=True)
    souls = models.CharField(max_length=255, blank=True)
    equipments = models.CharField(max_length=255, blank=True)
    gems = models.CharField(max_length=255, blank=True)
    stuffs = models.CharField(max_length=255, blank=True)

    des = models.CharField(max_length=255, blank=True)
    occurred_at = models.DateTimeField()

    class Meta:
        db_table = 'log_resource'

