devtools::load_all()
library(multichainr)

mc_set_path("E:/multichain")
multichainr:::mc_get_bin_path()

# 1. Создаем новый чейн (только один раз)
mc_node_init("testchain")

# 2. Запускаем его
mc_node_start("testchain")

# 3. Теперь нам нужно получить доступы (user/pass/port)
# Мы уже написали mc_get_config ранее, помнишь?
config <- mc_get_config("testchain")

# 4. Подключаемся к RPC
conn <- mc_connect(
  port = config$port, 
  user = config$user, 
  password = config$password
)

# 5. Проверяем работу
info <- mc_get_info(conn)
print(info$nodeaddress)

mc_node_stop(conn)

mc_get_info(conn)







# 1. Запускаем всё
mc_node_start("testchain")
conn <- mc_connect(mc_get_config("testchain"))


my_address <- mc_get_info(conn)$nodeaddress
mc_list_permissions(conn)

# 1. Получаем список реальных кошельков ноды
addresses <- mc_get_addresses(conn)

# Берем первый доступный адрес
my_wallet_address <- addresses[1]

# 2. Теперь выпускаем ассет на этот адрес
mc_issue(conn, my_wallet_address, "rcoin", 1000000)

# 5. Дадим другому адресу (если он у тебя есть) право подключаться
mc_grant(conn, "другой_адрес", "connect,receive")

# 6. Отправим ему 500 R-Coin
mc_send_asset(conn, "другой_адрес", "rcoin", 500)
















# 1. Создаем стрим (теперь с правильными аргументами)
mc_create_stream(conn, "sensor_data")

# 2. Обязательно подписываемся на него (даже если сами создали)
mc_subscribe(conn, "sensor_data")

# 3. Публикуем данные
mc_publish(conn, "sensor_data", "test_key", "Hello R!")
mc_publish(conn, "sensor_data", "json_key", list(temp = 25.5, humidity = 60))

# 4. Читаем
table <- mc_list_stream_items(conn, "sensor_data")
print(table)

mc_list_assets(conn)



mc_node_stop(conn)


# WALLET -----------------------------------------------------------------------
library(multichainr)

mc_node_init("testchain")
mc_node_start("testchain")

# 1. Подключаемся к первому узлу (Источник)
conn_a <- mc_connect(mc_get_config("testchain"))

# 2. Выбираем адрес, который хотим экспортировать
# Допустим, возьмем первый доступный адрес на узле
my_address <- mc_get_addresses(conn_a)[1]
message("Exporting key for: ", my_address)

# 3. Экспортируем приватный ключ
priv_key <- mc_dump_privkey(conn_a, my_address)
message("Private key (WIF) obtained.")

# --- Переходим к другому узлу ---

# 4. Подключаемся ко второму узлу (Приемник)
conn_b <- mc_connect(mc_get_config("testchain"))

# 5. Импортируем ключ на второй узел
# rescan = TRUE (по умолчанию) заставит узел пересчитать блокчейн, 
# чтобы увидеть старые транзакции и баланс этого адреса.
message("Importing key to Node B...")

mc_import_private_key(
  conn_b, 
  privkey = priv_key, 
  label = "migrated_identity", 
  rescan = TRUE)

# 6. Проверяем, появился ли адрес в списке на втором узле
addresses_b <- mc_get_addresses(conn_b)
if (my_address %in% addresses_b) {
  message("Success! Node B now owns address: ", my_address)
} else {
  warning("Address not found on Node B.")
}

# 7. Проверяем баланс на новом узле для этого адреса
balances <- mc_get_address_balances(conn_b, my_address)
print(balances)

mc_node_stop(conn_a)

mc_node_start("testchain")
conn <- mc_connect(mc_get_config("testchain"))
(p <- mc_get_blockchain_params(conn))
p$`default-rpc-port`
(p <- mc_get_runtime_params(conn))
p$acceptfiltertimeout
mc_set_runtime_param(conn, "acceptfiltertimeout", 1000)
mc_get_runtime_params(conn)$acceptfiltertimeout
mc_node_stop("testchain")


mc_node_start("testchain")
conn <- mc_connect(mc_get_config("testchain"))
params <- mc_get_runtime_params(conn)
names(params)
mc_set_runtime_param(conn, "mining", TRUE)

# Проверяем, применилось ли изменение
current_params <- mc_get_runtime_params(conn)
if (current_params$mining) {
  message("Mining is now active.")
}
mc_node_stop("testchain")
