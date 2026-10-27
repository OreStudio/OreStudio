# Hand-written sources that collide in a unity build, kept out of it.
# Written by build/scripts/check_unity_collisions.py --write; the list
# only shrinks. Fix a collision by naming the helper for what it does or
# by sharing one definition, then remove the file here.
set_source_files_properties(
    "${CMAKE_SOURCE_DIR}/projects/ores.analytics/core/tests/repository_shift_vocabulary_seed_tests.cpp"
    DIRECTORY "${CMAKE_SOURCE_DIR}/projects/ores.analytics/core/tests"
    PROPERTIES SKIP_UNITY_BUILD_INCLUSION ON)
set_source_files_properties(
    "${CMAKE_SOURCE_DIR}/projects/ores.analytics/quant/src/service/processes/libor_market_model_process.cpp"
    "${CMAKE_SOURCE_DIR}/projects/ores.analytics/quant/src/service/processes/periodic_arrival_process.cpp"
    "${CMAKE_SOURCE_DIR}/projects/ores.analytics/quant/src/service/processes/poisson_arrival_process.cpp"
    "${CMAKE_SOURCE_DIR}/projects/ores.analytics/quant/src/service/processes/quadratic_gaussian_process.cpp"
    DIRECTORY "${CMAKE_SOURCE_DIR}/projects/ores.analytics/quant/src"
    PROPERTIES SKIP_UNITY_BUILD_INCLUSION ON)
set_source_files_properties(
    "${CMAKE_SOURCE_DIR}/projects/ores.analytics/quant/tests/affine_term_structure_process_tests.cpp"
    "${CMAKE_SOURCE_DIR}/projects/ores.analytics/quant/tests/black_karasinski_process_tests.cpp"
    "${CMAKE_SOURCE_DIR}/projects/ores.analytics/quant/tests/curve_bootstrap_engine_tests.cpp"
    "${CMAKE_SOURCE_DIR}/projects/ores.analytics/quant/tests/forward_rate_calculator_tests.cpp"
    "${CMAKE_SOURCE_DIR}/projects/ores.analytics/quant/tests/heath_jarrow_morton_process_tests.cpp"
    "${CMAKE_SOURCE_DIR}/projects/ores.analytics/quant/tests/libor_market_model_process_tests.cpp"
    "${CMAKE_SOURCE_DIR}/projects/ores.analytics/quant/tests/quadratic_gaussian_process_tests.cpp"
    "${CMAKE_SOURCE_DIR}/projects/ores.analytics/quant/tests/quantlib_calendar_rulesets_tests.cpp"
    "${CMAKE_SOURCE_DIR}/projects/ores.analytics/quant/tests/rate_reciprocator_tests.cpp"
    "${CMAKE_SOURCE_DIR}/projects/ores.analytics/quant/tests/risk_recentering_tests.cpp"
    DIRECTORY "${CMAKE_SOURCE_DIR}/projects/ores.analytics/quant/tests"
    PROPERTIES SKIP_UNITY_BUILD_INCLUSION ON)
set_source_files_properties(
    "${CMAKE_SOURCE_DIR}/projects/ores.compute/core/tests/repository_batch_stated_order_tests.cpp"
    "${CMAKE_SOURCE_DIR}/projects/ores.compute/core/tests/repository_result_outcome_filter_tests.cpp"
    "${CMAKE_SOURCE_DIR}/projects/ores.compute/core/tests/repository_result_timing_tests.cpp"
    "${CMAKE_SOURCE_DIR}/projects/ores.compute/core/tests/repository_work_ledger_repository_tests.cpp"
    DIRECTORY "${CMAKE_SOURCE_DIR}/projects/ores.compute/core/tests"
    PROPERTIES SKIP_UNITY_BUILD_INCLUSION ON)
set_source_files_properties(
    "${CMAKE_SOURCE_DIR}/projects/ores.database/src/service/tenant_context.cpp"
    DIRECTORY "${CMAKE_SOURCE_DIR}/projects/ores.database/src"
    PROPERTIES SKIP_UNITY_BUILD_INCLUSION ON)
set_source_files_properties(
    "${CMAKE_SOURCE_DIR}/projects/ores.iam/client/src/client/service_token_provider.cpp"
    "${CMAKE_SOURCE_DIR}/projects/ores.iam/client/src/client/storage_capability_minter.cpp"
    DIRECTORY "${CMAKE_SOURCE_DIR}/projects/ores.iam/client/src"
    PROPERTIES SKIP_UNITY_BUILD_INCLUSION ON)
set_source_files_properties(
    "${CMAKE_SOURCE_DIR}/projects/ores.iam/core/src/messaging/registrar.cpp"
    "${CMAKE_SOURCE_DIR}/projects/ores.iam/core/src/repository/tenant_lookups.cpp"
    "${CMAKE_SOURCE_DIR}/projects/ores.iam/core/src/service/internal_request_client.cpp"
    "${CMAKE_SOURCE_DIR}/projects/ores.iam/core/src/service/run_grant_operations_service.cpp"
    "${CMAKE_SOURCE_DIR}/projects/ores.iam/core/src/service/tenant_session_service.cpp"
    DIRECTORY "${CMAKE_SOURCE_DIR}/projects/ores.iam/core/src"
    PROPERTIES SKIP_UNITY_BUILD_INCLUSION ON)
set_source_files_properties(
    "${CMAKE_SOURCE_DIR}/projects/ores.iam/core/tests/repository_account_party_repository_tests.cpp"
    "${CMAKE_SOURCE_DIR}/projects/ores.iam/core/tests/repository_tenant_stated_order_tests.cpp"
    "${CMAKE_SOURCE_DIR}/projects/ores.iam/core/tests/service_member_role_tests.cpp"
    "${CMAKE_SOURCE_DIR}/projects/ores.iam/core/tests/service_run_grant_exchange_tests.cpp"
    "${CMAKE_SOURCE_DIR}/projects/ores.iam/core/tests/service_run_grant_operations_service_tests.cpp"
    "${CMAKE_SOURCE_DIR}/projects/ores.iam/core/tests/service_tenant_session_service_tests.cpp"
    DIRECTORY "${CMAKE_SOURCE_DIR}/projects/ores.iam/core/tests"
    PROPERTIES SKIP_UNITY_BUILD_INCLUSION ON)
set_source_files_properties(
    "${CMAKE_SOURCE_DIR}/projects/ores.inbox/core/src/service/notification_center.cpp"
    DIRECTORY "${CMAKE_SOURCE_DIR}/projects/ores.inbox/core/src"
    PROPERTIES SKIP_UNITY_BUILD_INCLUSION ON)
set_source_files_properties(
    "${CMAKE_SOURCE_DIR}/projects/ores.inbox/core/tests/approval_apply_retry_tests.cpp"
    "${CMAKE_SOURCE_DIR}/projects/ores.inbox/core/tests/approval_as_service_tests.cpp"
    "${CMAKE_SOURCE_DIR}/projects/ores.inbox/core/tests/approval_lifecycle_tests.cpp"
    "${CMAKE_SOURCE_DIR}/projects/ores.inbox/core/tests/notification_center_tests.cpp"
    DIRECTORY "${CMAKE_SOURCE_DIR}/projects/ores.inbox/core/tests"
    PROPERTIES SKIP_UNITY_BUILD_INCLUSION ON)
set_source_files_properties(
    "${CMAKE_SOURCE_DIR}/projects/ores.logging/tests/logging_options_validator_tests.cpp"
    DIRECTORY "${CMAKE_SOURCE_DIR}/projects/ores.logging/tests"
    PROPERTIES SKIP_UNITY_BUILD_INCLUSION ON)
set_source_files_properties(
    "${CMAKE_SOURCE_DIR}/projects/ores.marketdata/api/src/datum/market_index.cpp"
    "${CMAKE_SOURCE_DIR}/projects/ores.marketdata/api/src/datum/value.cpp"
    DIRECTORY "${CMAKE_SOURCE_DIR}/projects/ores.marketdata/api/src"
    PROPERTIES SKIP_UNITY_BUILD_INCLUSION ON)
set_source_files_properties(
    "${CMAKE_SOURCE_DIR}/projects/ores.marketdata/core/src/datum/ore_key_codec.cpp"
    "${CMAKE_SOURCE_DIR}/projects/ores.marketdata/core/src/datum/oresmd_uri_codec.cpp"
    "${CMAKE_SOURCE_DIR}/projects/ores.marketdata/core/src/service/series_evolution_reader.cpp"
    "${CMAKE_SOURCE_DIR}/projects/ores.marketdata/core/src/service/series_identity_lister.cpp"
    "${CMAKE_SOURCE_DIR}/projects/ores.marketdata/core/src/service/series_shape_writer.cpp"
    "${CMAKE_SOURCE_DIR}/projects/ores.marketdata/core/src/service/series_slice_reader.cpp"
    "${CMAKE_SOURCE_DIR}/projects/ores.marketdata/core/src/service/series_snapshot_reader.cpp"
    "${CMAKE_SOURCE_DIR}/projects/ores.marketdata/core/src/vendor/demo_notation.cpp"
    "${CMAKE_SOURCE_DIR}/projects/ores.marketdata/core/src/vendor/lseg_notation.cpp"
    DIRECTORY "${CMAKE_SOURCE_DIR}/projects/ores.marketdata/core/src"
    PROPERTIES SKIP_UNITY_BUILD_INCLUSION ON)
set_source_files_properties(
    "${CMAKE_SOURCE_DIR}/projects/ores.marketdata/core/tests/datum_ore_key_codec_tests.cpp"
    "${CMAKE_SOURCE_DIR}/projects/ores.marketdata/core/tests/datum_oresmd_uri_codec_tests.cpp"
    "${CMAKE_SOURCE_DIR}/projects/ores.marketdata/core/tests/messaging_series_ticks_handler_tests.cpp"
    "${CMAKE_SOURCE_DIR}/projects/ores.marketdata/core/tests/messaging_tick_read_party_tests.cpp"
    "${CMAKE_SOURCE_DIR}/projects/ores.marketdata/core/tests/repository_market_write_ceiling_tests.cpp"
    "${CMAKE_SOURCE_DIR}/projects/ores.marketdata/core/tests/repository_subscription_route_tests.cpp"
    "${CMAKE_SOURCE_DIR}/projects/ores.marketdata/core/tests/repository_tick_reads_tests.cpp"
    "${CMAKE_SOURCE_DIR}/projects/ores.marketdata/core/tests/service_import_service_tests.cpp"
    "${CMAKE_SOURCE_DIR}/projects/ores.marketdata/core/tests/service_series_evolution_reader_tests.cpp"
    "${CMAKE_SOURCE_DIR}/projects/ores.marketdata/core/tests/service_series_identity_lister_tests.cpp"
    "${CMAKE_SOURCE_DIR}/projects/ores.marketdata/core/tests/service_series_slice_reader_tests.cpp"
    "${CMAKE_SOURCE_DIR}/projects/ores.marketdata/core/tests/service_series_snapshot_reader_tests.cpp"
    "${CMAKE_SOURCE_DIR}/projects/ores.marketdata/core/tests/service_series_tick_reader_tests.cpp"
    DIRECTORY "${CMAKE_SOURCE_DIR}/projects/ores.marketdata/core/tests"
    PROPERTIES SKIP_UNITY_BUILD_INCLUSION ON)
set_source_files_properties(
    "${CMAKE_SOURCE_DIR}/projects/ores.marketdata/service/src/app/demand_calculator.cpp"
    "${CMAKE_SOURCE_DIR}/projects/ores.marketdata/service/src/app/feed_ingest_loop.cpp"
    "${CMAKE_SOURCE_DIR}/projects/ores.marketdata/service/src/app/tick_plan.cpp"
    "${CMAKE_SOURCE_DIR}/projects/ores.marketdata/service/src/app/usage_planner.cpp"
    "${CMAKE_SOURCE_DIR}/projects/ores.marketdata/service/src/app/usage_recorder.cpp"
    DIRECTORY "${CMAKE_SOURCE_DIR}/projects/ores.marketdata/service/src"
    PROPERTIES SKIP_UNITY_BUILD_INCLUSION ON)
set_source_files_properties(
    "${CMAKE_SOURCE_DIR}/projects/ores.marketdata/service/tests/app_adapter_tick_translator_tests.cpp"
    "${CMAKE_SOURCE_DIR}/projects/ores.marketdata/service/tests/app_billing_tests.cpp"
    "${CMAKE_SOURCE_DIR}/projects/ores.marketdata/service/tests/app_composite_engine_tests.cpp"
    "${CMAKE_SOURCE_DIR}/projects/ores.marketdata/service/tests/app_crm_ingest_bridge_tests.cpp"
    "${CMAKE_SOURCE_DIR}/projects/ores.marketdata/service/tests/app_feed_ingest_loop_tests.cpp"
    "${CMAKE_SOURCE_DIR}/projects/ores.marketdata/service/tests/app_feed_series_tests.cpp"
    "${CMAKE_SOURCE_DIR}/projects/ores.marketdata/service/tests/app_tick_plan_tests.cpp"
    "${CMAKE_SOURCE_DIR}/projects/ores.marketdata/service/tests/app_usage_planner_tests.cpp"
    "${CMAKE_SOURCE_DIR}/projects/ores.marketdata/service/tests/curve_republish_resolver_fomc_tests.cpp"
    "${CMAKE_SOURCE_DIR}/projects/ores.marketdata/service/tests/curve_republish_resolver_tests.cpp"
    DIRECTORY "${CMAKE_SOURCE_DIR}/projects/ores.marketdata/service/tests"
    PROPERTIES SKIP_UNITY_BUILD_INCLUSION ON)
set_source_files_properties(
    "${CMAKE_SOURCE_DIR}/projects/ores.nats/src/service/jetstream_admin.cpp"
    DIRECTORY "${CMAKE_SOURCE_DIR}/projects/ores.nats/src"
    PROPERTIES SKIP_UNITY_BUILD_INCLUSION ON)
set_source_files_properties(
    "${CMAKE_SOURCE_DIR}/projects/ores.nats/tests/service_message_ownership_tests.cpp"
    "${CMAKE_SOURCE_DIR}/projects/ores.nats/tests/service_subscription_lifetime_tests.cpp"
    DIRECTORY "${CMAKE_SOURCE_DIR}/projects/ores.nats/tests"
    PROPERTIES SKIP_UNITY_BUILD_INCLUSION ON)
set_source_files_properties(
    "${CMAKE_SOURCE_DIR}/projects/ores.ore/core/src/domain/bond_instrument_projection.cpp"
    "${CMAKE_SOURCE_DIR}/projects/ores.ore/core/src/domain/commodity_instrument_projection.cpp"
    "${CMAKE_SOURCE_DIR}/projects/ores.ore/core/src/domain/composite_instrument_projection.cpp"
    "${CMAKE_SOURCE_DIR}/projects/ores.ore/core/src/domain/conventions_family_average_ois.cpp"
    "${CMAKE_SOURCE_DIR}/projects/ores.ore/core/src/domain/conventions_family_bma_basis_swap.cpp"
    "${CMAKE_SOURCE_DIR}/projects/ores.ore/core/src/domain/conventions_family_bond_yield.cpp"
    "${CMAKE_SOURCE_DIR}/projects/ores.ore/core/src/domain/conventions_family_cds.cpp"
    "${CMAKE_SOURCE_DIR}/projects/ores.ore/core/src/domain/conventions_family_cms_spread_option.cpp"
    "${CMAKE_SOURCE_DIR}/projects/ores.ore/core/src/domain/conventions_mapper.cpp"
    "${CMAKE_SOURCE_DIR}/projects/ores.ore/core/src/domain/conventions_package.cpp"
    "${CMAKE_SOURCE_DIR}/projects/ores.ore/core/src/domain/credit_instrument_projection.cpp"
    "${CMAKE_SOURCE_DIR}/projects/ores.ore/core/src/domain/credit_simulation_projection.cpp"
    "${CMAKE_SOURCE_DIR}/projects/ores.ore/core/src/domain/curve_configuration_projection.cpp"
    "${CMAKE_SOURCE_DIR}/projects/ores.ore/core/src/domain/equity_instrument_projection.cpp"
    "${CMAKE_SOURCE_DIR}/projects/ores.ore/core/src/domain/fx_instrument_projection.cpp"
    "${CMAKE_SOURCE_DIR}/projects/ores.ore/core/src/domain/portfolio_projection.cpp"
    "${CMAKE_SOURCE_DIR}/projects/ores.ore/core/src/domain/pricing_engine_projection.cpp"
    "${CMAKE_SOURCE_DIR}/projects/ores.ore/core/src/domain/rate_instrument_projection.cpp"
    "${CMAKE_SOURCE_DIR}/projects/ores.ore/core/src/domain/run_document_projection.cpp"
    "${CMAKE_SOURCE_DIR}/projects/ores.ore/core/src/domain/scripted_instrument_projection.cpp"
    "${CMAKE_SOURCE_DIR}/projects/ores.ore/core/src/domain/todays_market_projection.cpp"
    "${CMAKE_SOURCE_DIR}/projects/ores.ore/core/src/xml/emitter.cpp"
    "${CMAKE_SOURCE_DIR}/projects/ores.ore/core/src/xml/importer.cpp"
    "${CMAKE_SOURCE_DIR}/projects/ores.ore/core/src/xml/record.cpp"
    "${CMAKE_SOURCE_DIR}/projects/ores.ore/core/src/xml/record_diff.cpp"
    DIRECTORY "${CMAKE_SOURCE_DIR}/projects/ores.ore/core/src"
    PROPERTIES SKIP_UNITY_BUILD_INCLUSION ON)
set_source_files_properties(
    "${CMAKE_SOURCE_DIR}/projects/ores.ore/core/tests/planner_import_planner_tests.cpp"
    "${CMAKE_SOURCE_DIR}/projects/ores.ore/core/tests/pricing_engine_database_roundtrip_tests.cpp"
    "${CMAKE_SOURCE_DIR}/projects/ores.ore/core/tests/run_store_database_tests.cpp"
    "${CMAKE_SOURCE_DIR}/projects/ores.ore/core/tests/todays_market_database_roundtrip_tests.cpp"
    "${CMAKE_SOURCE_DIR}/projects/ores.ore/core/tests/xml_corpus_tests.cpp"
    "${CMAKE_SOURCE_DIR}/projects/ores.ore/core/tests/xml_creditsimulation_projection_tests.cpp"
    "${CMAKE_SOURCE_DIR}/projects/ores.ore/core/tests/xml_curve_configuration_projection_roundtrip_tests.cpp"
    "${CMAKE_SOURCE_DIR}/projects/ores.ore/core/tests/xml_emitter_tests.cpp"
    "${CMAKE_SOURCE_DIR}/projects/ores.ore/core/tests/xml_exporter_directory_roundtrip_tests.cpp"
    "${CMAKE_SOURCE_DIR}/projects/ores.ore/core/tests/xml_exporter_tests.cpp"
    "${CMAKE_SOURCE_DIR}/projects/ores.ore/core/tests/xml_fx_convention_mapper_roundtrip_tests.cpp"
    "${CMAKE_SOURCE_DIR}/projects/ores.ore/core/tests/xml_importer_tests.cpp"
    "${CMAKE_SOURCE_DIR}/projects/ores.ore/core/tests/xml_ore_run_document_projection_roundtrip_tests.cpp"
    "${CMAKE_SOURCE_DIR}/projects/ores.ore/core/tests/xml_rates_corpus_roundtrip_tests.cpp"
    "${CMAKE_SOURCE_DIR}/projects/ores.ore/core/tests/xml_roundtrip_conventions_tests.cpp"
    "${CMAKE_SOURCE_DIR}/projects/ores.ore/core/tests/xml_roundtrip_harness_tests.cpp"
    "${CMAKE_SOURCE_DIR}/projects/ores.ore/core/tests/xml_roundtrip_reference_tests.cpp"
    "${CMAKE_SOURCE_DIR}/projects/ores.ore/core/tests/xml_roundtrip_values_tests.cpp"
    "${CMAKE_SOURCE_DIR}/projects/ores.ore/core/tests/xml_todays_market_projection_tests.cpp"
    "${CMAKE_SOURCE_DIR}/projects/ores.ore/core/tests/xml_trade_import_tests.cpp"
    DIRECTORY "${CMAKE_SOURCE_DIR}/projects/ores.ore/core/tests"
    PROPERTIES SKIP_UNITY_BUILD_INCLUSION ON)
set_source_files_properties(
    "${CMAKE_SOURCE_DIR}/projects/ores.ore/service/src/messaging/ore_import_execute_handler.cpp"
    "${CMAKE_SOURCE_DIR}/projects/ores.ore/service/src/messaging/run_configuration_operations.cpp"
    DIRECTORY "${CMAKE_SOURCE_DIR}/projects/ores.ore/service/src"
    PROPERTIES SKIP_UNITY_BUILD_INCLUSION ON)
set_source_files_properties(
    "${CMAKE_SOURCE_DIR}/projects/ores.platform/tests/time_datetime_tests.cpp"
    "${CMAKE_SOURCE_DIR}/projects/ores.platform/tests/time_utils_tests.cpp"
    DIRECTORY "${CMAKE_SOURCE_DIR}/projects/ores.platform/tests"
    PROPERTIES SKIP_UNITY_BUILD_INCLUSION ON)
set_source_files_properties(
    "${CMAKE_SOURCE_DIR}/projects/ores.refdata/api/tests/domain_tenor_resolution_fomc_tests.cpp"
    DIRECTORY "${CMAKE_SOURCE_DIR}/projects/ores.refdata/api/tests"
    PROPERTIES SKIP_UNITY_BUILD_INCLUSION ON)
set_source_files_properties(
    "${CMAKE_SOURCE_DIR}/projects/ores.refdata/core/src/messaging/calendar_materialisation_handler.cpp"
    "${CMAKE_SOURCE_DIR}/projects/ores.refdata/core/src/messaging/calendar_materialisation_registrar.cpp"
    "${CMAKE_SOURCE_DIR}/projects/ores.refdata/core/src/messaging/configuration_document_registrar.cpp"
    "${CMAKE_SOURCE_DIR}/projects/ores.refdata/core/src/messaging/publish_from_dq_handler.cpp"
    "${CMAKE_SOURCE_DIR}/projects/ores.refdata/core/src/messaging/registrar.cpp"
    "${CMAKE_SOURCE_DIR}/projects/ores.refdata/core/src/service/conventions_bundle_service.cpp"
    "${CMAKE_SOURCE_DIR}/projects/ores.refdata/core/src/service/conventions_identity.cpp"
    "${CMAKE_SOURCE_DIR}/projects/ores.refdata/core/src/service/counterparty_identifier_service.cpp"
    "${CMAKE_SOURCE_DIR}/projects/ores.refdata/core/src/service/counterparty_service.cpp"
    DIRECTORY "${CMAKE_SOURCE_DIR}/projects/ores.refdata/core/src"
    PROPERTIES SKIP_UNITY_BUILD_INCLUSION ON)
set_source_files_properties(
    "${CMAKE_SOURCE_DIR}/projects/ores.refdata/core/tests/conventions_bundle_service_tests.cpp"
    "${CMAKE_SOURCE_DIR}/projects/ores.refdata/core/tests/repository_currency_country_repository_tests.cpp"
    "${CMAKE_SOURCE_DIR}/projects/ores.refdata/core/tests/repository_currency_pair_convention_calendar_repository_tests.cpp"
    "${CMAKE_SOURCE_DIR}/projects/ores.refdata/core/tests/repository_party_counterparty_repository_tests.cpp"
    "${CMAKE_SOURCE_DIR}/projects/ores.refdata/core/tests/repository_party_country_repository_tests.cpp"
    "${CMAKE_SOURCE_DIR}/projects/ores.refdata/core/tests/repository_party_currency_repository_tests.cpp"
    "${CMAKE_SOURCE_DIR}/projects/ores.refdata/core/tests/service_calendar_materialisation_service_tests.cpp"
    "${CMAKE_SOURCE_DIR}/projects/ores.refdata/core/tests/service_counterparty_gate_tests.cpp"
    "${CMAKE_SOURCE_DIR}/projects/ores.refdata/core/tests/service_tenor_resolution_service_tests.cpp"
    "${CMAKE_SOURCE_DIR}/projects/ores.refdata/core/tests/workflow_counterparty_apply_workflow_tests.cpp"
    DIRECTORY "${CMAKE_SOURCE_DIR}/projects/ores.refdata/core/tests"
    PROPERTIES SKIP_UNITY_BUILD_INCLUSION ON)
set_source_files_properties(
    "${CMAKE_SOURCE_DIR}/projects/ores.refdata/service/src/service/book_proposal.cpp"
    "${CMAKE_SOURCE_DIR}/projects/ores.refdata/service/src/service/counterparty_proposal.cpp"
    DIRECTORY "${CMAKE_SOURCE_DIR}/projects/ores.refdata/service/src"
    PROPERTIES SKIP_UNITY_BUILD_INCLUSION ON)
set_source_files_properties(
    "${CMAKE_SOURCE_DIR}/projects/ores.reporting/core/tests/run_document_handler_tests.cpp"
    "${CMAKE_SOURCE_DIR}/projects/ores.reporting/core/tests/run_document_service_tests.cpp"
    "${CMAKE_SOURCE_DIR}/projects/ores.reporting/core/tests/service_next_fire_tests.cpp"
    "${CMAKE_SOURCE_DIR}/projects/ores.reporting/core/tests/service_report_error_counts_tests.cpp"
    "${CMAKE_SOURCE_DIR}/projects/ores.reporting/core/tests/service_report_error_recorder_tests.cpp"
    "${CMAKE_SOURCE_DIR}/projects/ores.reporting/core/tests/service_report_instance_listing_tests.cpp"
    "${CMAKE_SOURCE_DIR}/projects/ores.reporting/core/tests/service_report_run_admission_tests.cpp"
    "${CMAKE_SOURCE_DIR}/projects/ores.reporting/core/tests/service_report_run_grant_tests.cpp"
    DIRECTORY "${CMAKE_SOURCE_DIR}/projects/ores.reporting/core/tests"
    PROPERTIES SKIP_UNITY_BUILD_INCLUSION ON)
set_source_files_properties(
    "${CMAKE_SOURCE_DIR}/projects/ores.scheduler/core/src/service/nats_publish_action_handler.cpp"
    "${CMAKE_SOURCE_DIR}/projects/ores.scheduler/core/src/service/scheduler_loop.cpp"
    "${CMAKE_SOURCE_DIR}/projects/ores.scheduler/core/src/service/sql_action_handler.cpp"
    DIRECTORY "${CMAKE_SOURCE_DIR}/projects/ores.scheduler/core/src"
    PROPERTIES SKIP_UNITY_BUILD_INCLUSION ON)
set_source_files_properties(
    "${CMAKE_SOURCE_DIR}/projects/ores.scheduler/core/tests/service_nats_publish_action_handler_tests.cpp"
    "${CMAKE_SOURCE_DIR}/projects/ores.scheduler/core/tests/service_schedule_decision_tests.cpp"
    "${CMAKE_SOURCE_DIR}/projects/ores.scheduler/core/tests/service_sql_action_handler_tests.cpp"
    DIRECTORY "${CMAKE_SOURCE_DIR}/projects/ores.scheduler/core/tests"
    PROPERTIES SKIP_UNITY_BUILD_INCLUSION ON)
set_source_files_properties(
    "${CMAKE_SOURCE_DIR}/projects/ores.shell/application/src/app/commands/scheduler_commands.cpp"
    "${CMAKE_SOURCE_DIR}/projects/ores.shell/application/src/app/commands/script_commands.cpp"
    "${CMAKE_SOURCE_DIR}/projects/ores.shell/application/src/app/commands/synthetic_commands.cpp"
    "${CMAKE_SOURCE_DIR}/projects/ores.shell/application/src/app/commands/trading/ore_commands.cpp"
    DIRECTORY "${CMAKE_SOURCE_DIR}/projects/ores.shell/application/src"
    PROPERTIES SKIP_UNITY_BUILD_INCLUSION ON)
set_source_files_properties(
    "${CMAKE_SOURCE_DIR}/projects/ores.storage/core/tests/http_client_tests.cpp"
    "${CMAKE_SOURCE_DIR}/projects/ores.storage/core/tests/storage_transfer_tests.cpp"
    DIRECTORY "${CMAKE_SOURCE_DIR}/projects/ores.storage/core/tests"
    PROPERTIES SKIP_UNITY_BUILD_INCLUSION ON)
set_source_files_properties(
    "${CMAKE_SOURCE_DIR}/projects/ores.synthetic/api/src/feeds/ir_curve_feed.cpp"
    "${CMAKE_SOURCE_DIR}/projects/ores.synthetic/api/src/feeds/ir_parameter_conversion.cpp"
    DIRECTORY "${CMAKE_SOURCE_DIR}/projects/ores.synthetic/api/src"
    PROPERTIES SKIP_UNITY_BUILD_INCLUSION ON)
set_source_files_properties(
    "${CMAKE_SOURCE_DIR}/projects/ores.synthetic/api/tests/feed_overview_tests.cpp"
    "${CMAKE_SOURCE_DIR}/projects/ores.synthetic/api/tests/ir_parameter_conversion_tests.cpp"
    DIRECTORY "${CMAKE_SOURCE_DIR}/projects/ores.synthetic/api/tests"
    PROPERTIES SKIP_UNITY_BUILD_INCLUSION ON)
set_source_files_properties(
    "${CMAKE_SOURCE_DIR}/projects/ores.synthetic/service/src/feed_kind_registry.cpp"
    "${CMAKE_SOURCE_DIR}/projects/ores.synthetic/service/src/registrar.cpp"
    "${CMAKE_SOURCE_DIR}/projects/ores.synthetic/service/src/simulator_adapter.cpp"
    DIRECTORY "${CMAKE_SOURCE_DIR}/projects/ores.synthetic/service/src"
    PROPERTIES SKIP_UNITY_BUILD_INCLUSION ON)
set_source_files_properties(
    "${CMAKE_SOURCE_DIR}/projects/ores.synthetic/service/tests/feed_kind_registry_tests.cpp"
    "${CMAKE_SOURCE_DIR}/projects/ores.synthetic/service/tests/folder_feed_control_handler_tests.cpp"
    "${CMAKE_SOURCE_DIR}/projects/ores.synthetic/service/tests/simulator_faults_tests.cpp"
    DIRECTORY "${CMAKE_SOURCE_DIR}/projects/ores.synthetic/service/tests"
    PROPERTIES SKIP_UNITY_BUILD_INCLUSION ON)
set_source_files_properties(
    "${CMAKE_SOURCE_DIR}/projects/ores.telemetry/core/src/log/telemetry_sink_backend.cpp"
    DIRECTORY "${CMAKE_SOURCE_DIR}/projects/ores.telemetry/core/src"
    PROPERTIES SKIP_UNITY_BUILD_INCLUSION ON)
set_source_files_properties(
    "${CMAKE_SOURCE_DIR}/projects/ores.telemetry/service/src/app/nats_poller.cpp"
    DIRECTORY "${CMAKE_SOURCE_DIR}/projects/ores.telemetry/service/src"
    PROPERTIES SKIP_UNITY_BUILD_INCLUSION ON)
set_source_files_properties(
    "${CMAKE_SOURCE_DIR}/projects/ores.trading/api/tests/domain_trade_economic_digest_tests.cpp"
    DIRECTORY "${CMAKE_SOURCE_DIR}/projects/ores.trading/api/tests"
    PROPERTIES SKIP_UNITY_BUILD_INCLUSION ON)
set_source_files_properties(
    "${CMAKE_SOURCE_DIR}/projects/ores.trading/core/src/repository/trade_component_queries.cpp"
    "${CMAKE_SOURCE_DIR}/projects/ores.trading/core/src/service/trade_export_service.cpp"
    DIRECTORY "${CMAKE_SOURCE_DIR}/projects/ores.trading/core/src"
    PROPERTIES SKIP_UNITY_BUILD_INCLUSION ON)
set_source_files_properties(
    "${CMAKE_SOURCE_DIR}/projects/ores.trading/core/tests/repository_equity_asian_option_instrument_repository_tests.cpp"
    "${CMAKE_SOURCE_DIR}/projects/ores.trading/core/tests/repository_equity_barrier_option_instrument_repository_tests.cpp"
    "${CMAKE_SOURCE_DIR}/projects/ores.trading/core/tests/repository_equity_digital_option_instrument_repository_tests.cpp"
    "${CMAKE_SOURCE_DIR}/projects/ores.trading/core/tests/repository_equity_forward_instrument_repository_tests.cpp"
    "${CMAKE_SOURCE_DIR}/projects/ores.trading/core/tests/repository_equity_option_instrument_repository_tests.cpp"
    "${CMAKE_SOURCE_DIR}/projects/ores.trading/core/tests/repository_equity_position_instrument_repository_tests.cpp"
    "${CMAKE_SOURCE_DIR}/projects/ores.trading/core/tests/repository_equity_swap_instrument_repository_tests.cpp"
    "${CMAKE_SOURCE_DIR}/projects/ores.trading/core/tests/repository_equity_variance_swap_instrument_repository_tests.cpp"
    "${CMAKE_SOURCE_DIR}/projects/ores.trading/core/tests/repository_fx_accumulator_instrument_repository_tests.cpp"
    "${CMAKE_SOURCE_DIR}/projects/ores.trading/core/tests/repository_fx_asian_forward_instrument_repository_tests.cpp"
    "${CMAKE_SOURCE_DIR}/projects/ores.trading/core/tests/repository_fx_barrier_option_instrument_repository_tests.cpp"
    "${CMAKE_SOURCE_DIR}/projects/ores.trading/core/tests/repository_fx_digital_option_instrument_repository_tests.cpp"
    "${CMAKE_SOURCE_DIR}/projects/ores.trading/core/tests/repository_fx_forward_instrument_repository_tests.cpp"
    "${CMAKE_SOURCE_DIR}/projects/ores.trading/core/tests/repository_fx_vanilla_option_instrument_repository_tests.cpp"
    "${CMAKE_SOURCE_DIR}/projects/ores.trading/core/tests/repository_fx_variance_swap_instrument_repository_tests.cpp"
    "${CMAKE_SOURCE_DIR}/projects/ores.trading/core/tests/repository_trade_filter_tests.cpp"
    "${CMAKE_SOURCE_DIR}/projects/ores.trading/core/tests/repository_trade_summary_tests.cpp"
    "${CMAKE_SOURCE_DIR}/projects/ores.trading/core/tests/service_bond_instrument_reader_tests.cpp"
    DIRECTORY "${CMAKE_SOURCE_DIR}/projects/ores.trading/core/tests"
    PROPERTIES SKIP_UNITY_BUILD_INCLUSION ON)
set_source_files_properties(
    "${CMAKE_SOURCE_DIR}/projects/ores.workflow/core/src/messaging/registrar.cpp"
    DIRECTORY "${CMAKE_SOURCE_DIR}/projects/ores.workflow/core/src"
    PROPERTIES SKIP_UNITY_BUILD_INCLUSION ON)
