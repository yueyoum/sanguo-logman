from django.db import models

class Log(models.Model):
    levelname = models.CharField(max_length=16)
    node_id = models.IntegerField()
    error_id = models.IntegerField(db_index=True)
    char_id = models.IntegerField()
    func_name = models.CharField(max_length=64)
    msg = models.TextField()
    occurred_at = models.DateTimeField()

    class Meta:
        db_table = 'logs'
