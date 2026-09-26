B02, B03 and B04 survey for ores.trading, condensed
Measured 2026-09-26 at b79609d808.

$ projects/ores.codegen/venv/bin/python projects/ores.codegen/scripts/survey_component.py --component trading-cpp

--- B02, models by metatype ---
  component: 1 model(s), variability no
  entity: 57 model(s), variability yes
  field_group: 6 model(s), variability no
  module: 1 model(s), variability no
  Total models: 65. Variability-carrying models: 57.
  Excluded as variability-free: `component`, `field_group`, `module`.
  Modeling directory: `projects/ores.trading/modeling` (65 of 65 counted from `*.org` and `*/*.org`).

--- B03, hand-written C++ classification ---
  35 of 1763 C++ files are generated (2.0%); 1728 are hand-written.
  Hand-written by classification: generatable 1593, infrastructure 71, dead 0, unclassified 64.
  Dead: 0 file(s). This survey reports no file as dead from its path alone; a dead file is found by checking references.
  ## B04 Protocol inventory

  Hand-written files by area:
      459  api/domain
      386  core/repository
      218  core/messaging
      128  core/service
      120  service/messaging
      112  core/presentation
       74  api/generators
       60  api/eventing
       50  api/generator
       48  api/messaging
       44  core
       14  api
        5  service/app
        5  service/config
        5  service

--- B04, protocol inventory ---
  Protocol headers under api/include/ores.trading.api/messaging/: 49
  Of those, generated: 1 (trade_protocol.hpp)
  Hand-written with no model twin: instrument_protocol.hpp (743 lines)
  249 subject literal(s) in sources; 0 of them sit outside a protocol header.
  Subjects declared in any model: none.
  The core/modeling/ores.trading.protocol.org names four trade subjects in prose, which is a reference, not a declaration.

  Distinct subjects in code (249):
    trading.v1.activity_types.list
    trading.v1.ascots.delete
    trading.v1.ascots.history
    trading.v1.ascots.list
    trading.v1.ascots.save
    trading.v1.balance_guaranteed_swap_instruments.delete
    trading.v1.balance_guaranteed_swap_instruments.history
    trading.v1.balance_guaranteed_swap_instruments.list
    trading.v1.balance_guaranteed_swap_instruments.save
    trading.v1.bond_forwards.delete
    trading.v1.bond_forwards.history
    trading.v1.bond_forwards.list
    trading.v1.bond_forwards.save
    trading.v1.bond_future_delivery_baskets.delete
    trading.v1.bond_future_delivery_baskets.history
    trading.v1.bond_future_delivery_baskets.list
    trading.v1.bond_future_delivery_baskets.save
    trading.v1.bond_futures.delete
    trading.v1.bond_futures.history
    trading.v1.bond_futures.list
    trading.v1.bond_futures.save
    trading.v1.bond_instruments.delete
    trading.v1.bond_instruments.history
    trading.v1.bond_instruments.list
    trading.v1.bond_instruments.save
    trading.v1.bond_issue_call_dates.delete
    trading.v1.bond_issue_call_dates.history
    trading.v1.bond_issue_call_dates.list
    trading.v1.bond_issue_call_dates.save
    trading.v1.bond_issue_conversion_targets.delete
    trading.v1.bond_issue_conversion_targets.history
    trading.v1.bond_issue_conversion_targets.list
    trading.v1.bond_issue_conversion_targets.save
    trading.v1.bond_issues.delete
    trading.v1.bond_issues.history
    trading.v1.bond_issues.list
    trading.v1.bond_issues.save
    trading.v1.bond_leg_amortizations.delete
    trading.v1.bond_leg_amortizations.history
    trading.v1.bond_leg_amortizations.list
    trading.v1.bond_leg_amortizations.save
    trading.v1.bond_leg_amounts.delete
    trading.v1.bond_leg_amounts.history
    trading.v1.bond_leg_amounts.list
    trading.v1.bond_leg_amounts.save
    trading.v1.bond_leg_rates.delete
    trading.v1.bond_leg_rates.history
    trading.v1.bond_leg_rates.list
    trading.v1.bond_leg_rates.save
    trading.v1.bond_legs.delete
    trading.v1.bond_legs.history
    trading.v1.bond_legs.list
    trading.v1.bond_legs.save
    trading.v1.bond_options.delete
    trading.v1.bond_options.history
    trading.v1.bond_options.list
    trading.v1.bond_options.save
    trading.v1.bond_repos.delete
    trading.v1.bond_repos.history
    trading.v1.bond_repos.list
    trading.v1.bond_repos.save
    trading.v1.bond_trs.delete
    trading.v1.bond_trs.history
    trading.v1.bond_trs.list
    trading.v1.bond_trs.save
    trading.v1.callable_swap_instruments.delete
    trading.v1.callable_swap_instruments.history
    trading.v1.callable_swap_instruments.list
    trading.v1.callable_swap_instruments.save
    trading.v1.cap_floor_instruments.delete
    trading.v1.cap_floor_instruments.history
    trading.v1.cap_floor_instruments.list
    trading.v1.cap_floor_instruments.save
    trading.v1.commodity_instruments.delete
    trading.v1.commodity_instruments.history
    trading.v1.commodity_instruments.list
    trading.v1.commodity_instruments.save
    trading.v1.composite_instruments.delete
    trading.v1.composite_instruments.history
    trading.v1.composite_instruments.legs.list
    trading.v1.composite_instruments.list
    trading.v1.composite_instruments.save
    trading.v1.credit_instruments.delete
    trading.v1.credit_instruments.history
    trading.v1.credit_instruments.list
    trading.v1.credit_instruments.save
    trading.v1.equity_accumulator_instruments.delete
    trading.v1.equity_accumulator_instruments.history
    trading.v1.equity_accumulator_instruments.list
    trading.v1.equity_accumulator_instruments.save
    trading.v1.equity_asian_option_instruments.delete
    trading.v1.equity_asian_option_instruments.history
    trading.v1.equity_asian_option_instruments.list
    trading.v1.equity_asian_option_instruments.save
    trading.v1.equity_barrier_option_instruments.delete
    trading.v1.equity_barrier_option_instruments.history
    trading.v1.equity_barrier_option_instruments.list
    trading.v1.equity_barrier_option_instruments.save
    trading.v1.equity_digital_option_instruments.delete
    trading.v1.equity_digital_option_instruments.history
    trading.v1.equity_digital_option_instruments.list
    trading.v1.equity_digital_option_instruments.save
    trading.v1.equity_forward_instruments.delete
    trading.v1.equity_forward_instruments.history
    trading.v1.equity_forward_instruments.list
    trading.v1.equity_forward_instruments.save
    trading.v1.equity_option_instruments.delete
    trading.v1.equity_option_instruments.history
    trading.v1.equity_option_instruments.list
    trading.v1.equity_option_instruments.save
    trading.v1.equity_position_instruments.delete
    trading.v1.equity_position_instruments.history
    trading.v1.equity_position_instruments.list
    trading.v1.equity_position_instruments.save
    trading.v1.equity_swap_instruments.delete
    trading.v1.equity_swap_instruments.history
    trading.v1.equity_swap_instruments.list
    trading.v1.equity_swap_instruments.save
    trading.v1.equity_variance_swap_instruments.delete
    trading.v1.equity_variance_swap_instruments.history
    trading.v1.equity_variance_swap_instruments.list
    trading.v1.equity_variance_swap_instruments.save
    trading.v1.fra_instruments.delete
    trading.v1.fra_instruments.history
    trading.v1.fra_instruments.list
    trading.v1.fra_instruments.save
    trading.v1.fx_accumulator_instruments.delete
    trading.v1.fx_accumulator_instruments.history
    trading.v1.fx_accumulator_instruments.list
    trading.v1.fx_accumulator_instruments.save
    trading.v1.fx_asian_forward_instruments.delete
    trading.v1.fx_asian_forward_instruments.history
    trading.v1.fx_asian_forward_instruments.list
    trading.v1.fx_asian_forward_instruments.save
    trading.v1.fx_barrier_option_instruments.delete
    trading.v1.fx_barrier_option_instruments.history
    trading.v1.fx_barrier_option_instruments.list
    trading.v1.fx_barrier_option_instruments.save
    trading.v1.fx_digital_option_instruments.delete
    trading.v1.fx_digital_option_instruments.history
    trading.v1.fx_digital_option_instruments.list
    trading.v1.fx_digital_option_instruments.save
    trading.v1.fx_forward_instruments.delete
    trading.v1.fx_forward_instruments.history
    trading.v1.fx_forward_instruments.list
    trading.v1.fx_forward_instruments.save
    trading.v1.fx_vanilla_option_instruments.delete
    trading.v1.fx_vanilla_option_instruments.history
    trading.v1.fx_vanilla_option_instruments.list
    trading.v1.fx_vanilla_option_instruments.save
    trading.v1.fx_variance_swap_instruments.delete
    trading.v1.fx_variance_swap_instruments.history
    trading.v1.fx_variance_swap_instruments.list
    trading.v1.fx_variance_swap_instruments.save
    trading.v1.inflation_swap_instruments.delete
    trading.v1.inflation_swap_instruments.history
    trading.v1.inflation_swap_instruments.list
    trading.v1.inflation_swap_instruments.save
    trading.v1.instrument_option_exercise_fees.delete
    trading.v1.instrument_option_exercise_fees.history
    trading.v1.instrument_option_exercise_fees.list
    trading.v1.instrument_option_exercise_fees.save
    trading.v1.instrument_option_payment_dates.delete
    trading.v1.instrument_option_payment_dates.history
    trading.v1.instrument_option_payment_dates.list
    trading.v1.instrument_option_payment_dates.save
    trading.v1.instrument_option_premiums.delete
    trading.v1.instrument_option_premiums.history
    trading.v1.instrument_option_premiums.list
    trading.v1.instrument_option_premiums.save
    trading.v1.instrument_options.delete
    trading.v1.instrument_options.history
    trading.v1.instrument_options.list
    trading.v1.instrument_options.save
    trading.v1.instrument_schedule_dates.delete
    trading.v1.instrument_schedule_dates.history
    trading.v1.instrument_schedule_dates.list
    trading.v1.instrument_schedule_dates.save
    trading.v1.instrument_schedules.delete
    trading.v1.instrument_schedules.history
    trading.v1.instrument_schedules.list
    trading.v1.instrument_schedules.save
    trading.v1.instrument_strikes.delete
    trading.v1.instrument_strikes.history
    trading.v1.instrument_strikes.list
    trading.v1.instrument_strikes.save
    trading.v1.knock_out_swap_instruments.delete
    trading.v1.knock_out_swap_instruments.history
    trading.v1.knock_out_swap_instruments.list
    trading.v1.knock_out_swap_instruments.save
    trading.v1.lifecycle_events.delete
    trading.v1.lifecycle_events.history
    trading.v1.lifecycle_events.list
    trading.v1.lifecycle_events.save
    trading.v1.party_role_types.delete
    trading.v1.party_role_types.history
    trading.v1.party_role_types.list
    trading.v1.party_role_types.save
    trading.v1.rpa_instruments.delete
    trading.v1.rpa_instruments.history
    trading.v1.rpa_instruments.list
    trading.v1.rpa_instruments.save
    trading.v1.scripted_instruments.delete
    trading.v1.scripted_instruments.history
    trading.v1.scripted_instruments.list
    trading.v1.scripted_instruments.save
    trading.v1.swaption_instruments.delete
    trading.v1.swaption_instruments.history
    trading.v1.swaption_instruments.list
    trading.v1.swaption_instruments.save
    trading.v1.trade_envelope_additional_fields.delete
    trading.v1.trade_envelope_additional_fields.history
    trading.v1.trade_envelope_additional_fields.list
    trading.v1.trade_envelope_additional_fields.save
    trading.v1.trade_envelope_portfolio_ids.delete
    trading.v1.trade_envelope_portfolio_ids.history
    trading.v1.trade_envelope_portfolio_ids.list
    trading.v1.trade_envelope_portfolio_ids.save
    trading.v1.trade_envelopes.delete
    trading.v1.trade_envelopes.history
    trading.v1.trade_envelopes.list
    trading.v1.trade_envelopes.save
    trading.v1.trade_id_types.delete
    trading.v1.trade_id_types.history
    trading.v1.trade_id_types.list
    trading.v1.trade_id_types.save
    trading.v1.trade_identifiers.delete
    trading.v1.trade_identifiers.history
    trading.v1.trade_identifiers.list
    trading.v1.trade_identifiers.save
    trading.v1.trade_party_roles.delete
    trading.v1.trade_party_roles.history
    trading.v1.trade_party_roles.list
    trading.v1.trade_party_roles.save
    trading.v1.trade_types.delete
    trading.v1.trade_types.history
    trading.v1.trade_types.list
    trading.v1.trade_types.save
    trading.v1.trades.delete
    trading.v1.trades.export-to-storage
    trading.v1.trades.history
    trading.v1.trades.instrument
    trading.v1.trades.list
    trading.v1.trades.portfolio.export
    trading.v1.trades.save
    trading.v1.vanilla_swap_instruments.delete
    trading.v1.vanilla_swap_instruments.history
    trading.v1.vanilla_swap_instruments.list
    trading.v1.vanilla_swap_instruments.save
