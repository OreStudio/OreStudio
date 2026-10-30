/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*-
 *
 * Copyright (C) 2026 Marco Craveiro <marco.craveiro@gmail.com>
 *
 * This program is free software; you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free Software
 * Foundation; either version 3 of the License, or (at your option) any later
 * version.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
 * details.
 *
 * You should have received a copy of the GNU General Public License along with
 * this program; if not, write to the Free Software Foundation, Inc., 51
 * Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
 *
 */
#include "ores.refdata.core/service/curve_configuration_document_service.hpp"
#include "ores.database/repository/document_operations.hpp"
#include "ores.refdata.api/messaging/configuration_document_protocol.hpp"
#include "ores.refdata.core/repository/base_correlation_config_repository.hpp"
#include "ores.refdata.core/repository/bond_future_volatility_config_repository.hpp"
#include "ores.refdata.core/repository/cap_floor_proxy_config_repository.hpp"
#include "ores.refdata.core/repository/cap_floor_volatility_config_repository.hpp"
#include "ores.refdata.core/repository/cds_volatility_config_repository.hpp"
#include "ores.refdata.core/repository/cds_volatility_term_repository.hpp"
#include "ores.refdata.core/repository/commodity_volatility_config_repository.hpp"
#include "ores.refdata.core/repository/curve_bootstrap_config_repository.hpp"
#include "ores.refdata.core/repository/curve_configuration_repository.hpp"
#include "ores.refdata.core/repository/curve_configuration_section_repository.hpp"
#include "ores.refdata.core/repository/curve_correlation_config_repository.hpp"
#include "ores.refdata.core/repository/curve_definition_repository.hpp"
#include "ores.refdata.core/repository/curve_global_report_repository.hpp"
#include "ores.refdata.core/repository/curve_parametric_smile_parameter_repository.hpp"
#include "ores.refdata.core/repository/curve_parametric_smile_repository.hpp"
#include "ores.refdata.core/repository/curve_quote_repository.hpp"
#include "ores.refdata.core/repository/curve_report_configuration_repository.hpp"
#include "ores.refdata.core/repository/curve_security_config_repository.hpp"
#include "ores.refdata.core/repository/curve_segment_curve_repository.hpp"
#include "ores.refdata.core/repository/curve_segment_repository.hpp"
#include "ores.refdata.core/repository/curve_volatility_config_repository.hpp"
#include "ores.refdata.core/repository/curve_volatility_constant_repository.hpp"
#include "ores.refdata.core/repository/curve_volatility_curve_repository.hpp"
#include "ores.refdata.core/repository/curve_volatility_delta_surface_repository.hpp"
#include "ores.refdata.core/repository/curve_volatility_kind_repository.hpp"
#include "ores.refdata.core/repository/curve_volatility_proxy_surface_repository.hpp"
#include "ores.refdata.core/repository/curve_volatility_strike_surface_repository.hpp"
#include "ores.refdata.core/repository/default_curve_config_repository.hpp"
#include "ores.refdata.core/repository/default_curve_configuration_repository.hpp"
#include "ores.refdata.core/repository/equity_curve_config_repository.hpp"
#include "ores.refdata.core/repository/equity_volatility_config_repository.hpp"
#include "ores.refdata.core/repository/fx_volatility_config_repository.hpp"
#include "ores.refdata.core/repository/fx_volatility_smile_repository.hpp"
#include "ores.refdata.core/repository/inflation_cap_floor_volatility_config_repository.hpp"
#include "ores.refdata.core/repository/inflation_cap_floor_volatility_strikes_repository.hpp"
#include "ores.refdata.core/repository/inflation_curve_config_repository.hpp"
#include "ores.refdata.core/repository/inflation_seasonality_factor_repository.hpp"
#include "ores.refdata.core/repository/intraday_power_curve_config_repository.hpp"
#include "ores.refdata.core/repository/swaption_proxy_config_repository.hpp"
#include "ores.refdata.core/repository/swaption_volatility_config_repository.hpp"
#include "ores.refdata.core/repository/yield_curve_config_repository.hpp"
#include "ores.service/messaging/handler_helpers.hpp"
#include "ores.utility/domain/protocol.hpp"
#include <boost/uuid/uuid.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <cstdint>
#include <optional>
#include <set>
#include <string>
#include <string_view>
#include <utility>
#include <vector>

namespace ores::refdata::service {

using namespace ores::refdata::repository;
using ores::database::repository::ids_of;
using ores::database::repository::write_amendment;
using ores::database::repository::read_one;
using ores::database::repository::read_where;

curve_configuration_document_service::curve_configuration_document_service(context ctx)
    : ctx_(std::move(ctx)) {}

void curve_configuration_document_service::save(messaging::curve_configuration_document v,
                                                std::string reason_code,
                                                std::string commentary) {
    ores::service::messaging::stamp_document(v, ctx_);
    const auto reason = std::string_view(reason_code);
    write_amendment(ctx_,
                    curve_configuration_repository(),
                    std::vector{std::move(v.config)},
                    reason,
                    commentary);
    write_amendment(
        ctx_, curve_configuration_section_repository(), std::move(v.sections), reason, commentary);
    write_amendment(
        ctx_, curve_global_report_repository(), std::move(v.global_reports), reason, commentary);
    write_amendment(
        ctx_, curve_definition_repository(), std::move(v.definitions), reason, commentary);
    write_amendment(
        ctx_, yield_curve_config_repository(), std::move(v.yield_curves), reason, commentary);
    write_amendment(
        ctx_, equity_curve_config_repository(), std::move(v.equity_curves), reason, commentary);
    write_amendment(
        ctx_, default_curve_config_repository(), std::move(v.default_curves), reason, commentary);
    write_amendment(ctx_,
                    default_curve_configuration_repository(),
                    std::move(v.default_curve_configurations),
                    reason,
                    commentary);
    write_amendment(ctx_,
                    inflation_curve_config_repository(),
                    std::move(v.inflation_curves),
                    reason,
                    commentary);
    write_amendment(ctx_,
                    inflation_seasonality_factor_repository(),
                    std::move(v.seasonality_factors),
                    reason,
                    commentary);
    write_amendment(
        ctx_, curve_security_config_repository(), std::move(v.securities), reason, commentary);
    write_amendment(ctx_,
                    intraday_power_curve_config_repository(),
                    std::move(v.intraday_power_curves),
                    reason,
                    commentary);
    write_amendment(
        ctx_, fx_volatility_config_repository(), std::move(v.fx_volatilities), reason, commentary);
    write_amendment(
        ctx_, fx_volatility_smile_repository(), std::move(v.fx_volatility_smiles), reason, commentary);
    write_amendment(ctx_,
                    base_correlation_config_repository(),
                    std::move(v.base_correlations),
                    reason,
                    commentary);
    write_amendment(
        ctx_, curve_correlation_config_repository(), std::move(v.correlations), reason, commentary);
    write_amendment(ctx_,
                    curve_report_configuration_repository(),
                    std::move(v.report_configurations),
                    reason,
                    commentary);
    write_amendment(ctx_,
                    cds_volatility_config_repository(),
                    std::move(v.cds_volatilities),
                    reason,
                    commentary);
    write_amendment(ctx_,
                    cds_volatility_term_repository(),
                    std::move(v.cds_volatility_terms),
                    reason,
                    commentary);
    write_amendment(ctx_,
                    curve_volatility_config_repository(),
                    std::move(v.volatility_configs),
                    reason,
                    commentary);
    write_amendment(ctx_,
                    curve_volatility_kind_repository(),
                    std::move(v.volatility_kinds),
                    reason,
                    commentary);
    write_amendment(ctx_,
                    curve_volatility_constant_repository(),
                    std::move(v.volatility_constants),
                    reason,
                    commentary);
    write_amendment(ctx_,
                    curve_volatility_curve_repository(),
                    std::move(v.volatility_curves),
                    reason,
                    commentary);
    write_amendment(ctx_,
                    curve_volatility_strike_surface_repository(),
                    std::move(v.volatility_strike_surfaces),
                    reason,
                    commentary);
    write_amendment(ctx_,
                    curve_volatility_delta_surface_repository(),
                    std::move(v.volatility_delta_surfaces),
                    reason,
                    commentary);
    write_amendment(ctx_,
                    curve_volatility_proxy_surface_repository(),
                    std::move(v.volatility_proxy_surfaces),
                    reason,
                    commentary);
    write_amendment(ctx_,
                    swaption_volatility_config_repository(),
                    std::move(v.swaption_volatilities),
                    reason,
                    commentary);
    write_amendment(ctx_,
                    swaption_proxy_config_repository(),
                    std::move(v.swaption_proxy_configs),
                    reason,
                    commentary);
    write_amendment(ctx_,
                    cap_floor_volatility_config_repository(),
                    std::move(v.cap_floor_volatilities),
                    reason,
                    commentary);
    write_amendment(ctx_,
                    cap_floor_proxy_config_repository(),
                    std::move(v.cap_floor_proxy_configs),
                    reason,
                    commentary);
    write_amendment(ctx_,
                    inflation_cap_floor_volatility_config_repository(),
                    std::move(v.inflation_cap_floor_volatilities),
                    reason,
                    commentary);
    write_amendment(ctx_,
                    inflation_cap_floor_volatility_strikes_repository(),
                    std::move(v.inflation_volatility_strikes),
                    reason,
                    commentary);
    write_amendment(ctx_,
                    curve_parametric_smile_repository(),
                    std::move(v.parametric_smiles),
                    reason,
                    commentary);
    write_amendment(ctx_,
                    curve_parametric_smile_parameter_repository(),
                    std::move(v.parametric_smile_parameters),
                    reason,
                    commentary);
    write_amendment(ctx_,
                    equity_volatility_config_repository(),
                    std::move(v.equity_volatilities),
                    reason,
                    commentary);
    write_amendment(ctx_,
                    commodity_volatility_config_repository(),
                    std::move(v.commodity_volatilities),
                    reason,
                    commentary);
    write_amendment(ctx_,
                    bond_future_volatility_config_repository(),
                    std::move(v.bond_future_volatilities),
                    reason,
                    commentary);
    write_amendment(ctx_,
                    curve_bootstrap_config_repository(),
                    std::move(v.bootstrap_configs),
                    reason,
                    commentary);
    write_amendment(ctx_, curve_segment_repository(), std::move(v.segments), reason, commentary);
    write_amendment(
        ctx_, curve_segment_curve_repository(), std::move(v.segment_curves), reason, commentary);
    write_amendment(ctx_, curve_quote_repository(), std::move(v.quotes), reason, commentary);
}

messaging::curve_configuration_document
curve_configuration_document_service::get(const boost::uuids::uuid& config_id) {
    messaging::curve_configuration_document r;
    r.config = read_one(ctx_, curve_configuration_repository(), "curve configuration", config_id);
    const auto of_config = [&](const auto& row) {
        return row.curve_configuration_id == config_id;
    };
    r.sections = read_where(ctx_, curve_configuration_section_repository(), of_config);
    r.global_reports = read_where(ctx_, curve_global_report_repository(), of_config);
    r.definitions = read_where(ctx_, curve_definition_repository(), of_config);
    std::set<boost::uuids::uuid> definitions;
    for (const auto& d : r.definitions)
        definitions.insert(d.id);
    const auto of_definition = [&](const auto& row) {
        return definitions.contains(row.curve_definition_id);
    };
    r.yield_curves = read_where(ctx_, yield_curve_config_repository(), of_definition);
    r.equity_curves = read_where(ctx_, equity_curve_config_repository(), of_definition);
    r.default_curves = read_where(ctx_, default_curve_config_repository(), of_definition);
    r.default_curve_configurations =
        read_where(ctx_, default_curve_configuration_repository(), of_definition);
    r.inflation_curves = read_where(ctx_, inflation_curve_config_repository(), of_definition);
    r.seasonality_factors =
        read_where(ctx_, inflation_seasonality_factor_repository(), of_definition);
    r.securities = read_where(ctx_, curve_security_config_repository(), of_definition);
    r.intraday_power_curves =
        read_where(ctx_, intraday_power_curve_config_repository(), of_definition);
    r.fx_volatilities = read_where(ctx_, fx_volatility_config_repository(), of_definition);
    std::set<boost::uuids::uuid> fx_volatility_entries;
    for (const auto& c : r.fx_volatilities)
        fx_volatility_entries.insert(c.id);
    r.fx_volatility_smiles = read_where(ctx_, fx_volatility_smile_repository(), [&](const auto& row) {
        return fx_volatility_entries.contains(row.fx_volatility_config_id);
    });
    r.base_correlations = read_where(ctx_, base_correlation_config_repository(), of_definition);
    r.correlations = read_where(ctx_, curve_correlation_config_repository(), of_definition);
    r.report_configurations =
        read_where(ctx_, curve_report_configuration_repository(), of_definition);
    r.cds_volatilities = read_where(ctx_, cds_volatility_config_repository(), of_definition);
    r.cds_volatility_terms = read_where(ctx_, cds_volatility_term_repository(), of_definition);
    r.volatility_configs = read_where(ctx_, curve_volatility_config_repository(), of_definition);
    std::set<boost::uuids::uuid> volatility_entry_ids;
    for (const auto& c : r.volatility_configs)
        volatility_entry_ids.insert(c.id);
    const auto of_volatility_entry = [&](const auto& row) {
        return volatility_entry_ids.contains(row.curve_volatility_config_id);
    };
    r.volatility_kinds = read_where(ctx_, curve_volatility_kind_repository(), of_volatility_entry);
    std::set<boost::uuids::uuid> volatility_kind_ids;
    for (const auto& k : r.volatility_kinds)
        volatility_kind_ids.insert(k.id);
    const auto of_volatility_kind = [&](const auto& row) {
        return volatility_kind_ids.contains(row.curve_volatility_kind_id);
    };
    r.volatility_constants =
        read_where(ctx_, curve_volatility_constant_repository(), of_volatility_kind);
    r.volatility_curves = read_where(ctx_, curve_volatility_curve_repository(), of_volatility_kind);
    r.volatility_strike_surfaces =
        read_where(ctx_, curve_volatility_strike_surface_repository(), of_volatility_kind);
    r.volatility_delta_surfaces =
        read_where(ctx_, curve_volatility_delta_surface_repository(), of_volatility_kind);
    r.volatility_proxy_surfaces =
        read_where(ctx_, curve_volatility_proxy_surface_repository(), of_volatility_kind);
    r.inflation_cap_floor_volatilities =
        read_where(ctx_, inflation_cap_floor_volatility_config_repository(), of_definition);
    std::set<boost::uuids::uuid> inflation_entries;
    for (const auto& c : r.inflation_cap_floor_volatilities)
        inflation_entries.insert(c.id);
    r.inflation_volatility_strikes =
        read_where(ctx_, inflation_cap_floor_volatility_strikes_repository(), [&](const auto& row) {
            return inflation_entries.contains(row.inflation_cap_floor_volatility_config_id);
        });
    r.swaption_volatilities =
        read_where(ctx_, swaption_volatility_config_repository(), of_definition);
    std::set<boost::uuids::uuid> swaption_entries;
    for (const auto& c : r.swaption_volatilities)
        swaption_entries.insert(c.id);
    const auto of_swaption_entry = [&](const auto& row) {
        return swaption_entries.contains(row.swaption_volatility_config_id);
    };
    r.swaption_proxy_configs =
        read_where(ctx_, swaption_proxy_config_repository(), of_swaption_entry);
    r.cap_floor_volatilities =
        read_where(ctx_, cap_floor_volatility_config_repository(), of_definition);
    std::set<boost::uuids::uuid> cap_floor_entries;
    for (const auto& c : r.cap_floor_volatilities)
        cap_floor_entries.insert(c.id);
    const auto of_cap_floor_entry = [&](const auto& row) {
        return cap_floor_entries.contains(row.cap_floor_volatility_config_id);
    };
    r.cap_floor_proxy_configs =
        read_where(ctx_, cap_floor_proxy_config_repository(), of_cap_floor_entry);
    r.parametric_smiles = read_where(ctx_, curve_parametric_smile_repository(), of_definition);
    r.parametric_smile_parameters =
        read_where(ctx_, curve_parametric_smile_parameter_repository(), of_definition);
    r.equity_volatilities = read_where(ctx_, equity_volatility_config_repository(), of_definition);
    r.commodity_volatilities =
        read_where(ctx_, commodity_volatility_config_repository(), of_definition);
    r.bond_future_volatilities =
        read_where(ctx_, bond_future_volatility_config_repository(), of_definition);
    r.bootstrap_configs = read_where(ctx_, curve_bootstrap_config_repository(), of_definition);
    r.segments = read_where(ctx_, curve_segment_repository(), of_definition);
    r.quotes = read_where(ctx_, curve_quote_repository(), of_definition);
    std::set<boost::uuids::uuid> segments;
    for (const auto& s : r.segments)
        segments.insert(s.id);
    r.segment_curves = read_where(ctx_, curve_segment_curve_repository(), [&](const auto& row) {
        return segments.contains(row.curve_segment_id);
    });
    return r;
}

void curve_configuration_document_service::remove(const boost::uuids::uuid& id) {
    const auto d = get(id);
    if (!d.quotes.empty())
        curve_quote_repository().remove(ctx_, ids_of(d.quotes));
    if (!d.segment_curves.empty())
        curve_segment_curve_repository().remove(ctx_, ids_of(d.segment_curves));
    if (!d.segments.empty())
        curve_segment_repository().remove(ctx_, ids_of(d.segments));
    if (!d.bootstrap_configs.empty())
        curve_bootstrap_config_repository().remove(ctx_, ids_of(d.bootstrap_configs));
    if (!d.bond_future_volatilities.empty())
        bond_future_volatility_config_repository().remove(ctx_, ids_of(d.bond_future_volatilities));
    if (!d.commodity_volatilities.empty())
        commodity_volatility_config_repository().remove(ctx_, ids_of(d.commodity_volatilities));
    if (!d.equity_volatilities.empty())
        equity_volatility_config_repository().remove(ctx_, ids_of(d.equity_volatilities));
    if (!d.parametric_smile_parameters.empty())
        curve_parametric_smile_parameter_repository().remove(ctx_,
                                                             ids_of(d.parametric_smile_parameters));
    if (!d.parametric_smiles.empty())
        curve_parametric_smile_repository().remove(ctx_, ids_of(d.parametric_smiles));
    if (!d.cap_floor_proxy_configs.empty())
        cap_floor_proxy_config_repository().remove(ctx_, ids_of(d.cap_floor_proxy_configs));
    if (!d.cap_floor_volatilities.empty())
        cap_floor_volatility_config_repository().remove(ctx_, ids_of(d.cap_floor_volatilities));
    if (!d.swaption_proxy_configs.empty())
        swaption_proxy_config_repository().remove(ctx_, ids_of(d.swaption_proxy_configs));
    if (!d.swaption_volatilities.empty())
        swaption_volatility_config_repository().remove(ctx_, ids_of(d.swaption_volatilities));
    if (!d.inflation_volatility_strikes.empty())
        inflation_cap_floor_volatility_strikes_repository().remove(
            ctx_, ids_of(d.inflation_volatility_strikes));
    if (!d.inflation_cap_floor_volatilities.empty())
        inflation_cap_floor_volatility_config_repository().remove(
            ctx_, ids_of(d.inflation_cap_floor_volatilities));
    if (!d.volatility_proxy_surfaces.empty())
        curve_volatility_proxy_surface_repository().remove(ctx_, ids_of(d.volatility_proxy_surfaces));
    if (!d.volatility_delta_surfaces.empty())
        curve_volatility_delta_surface_repository().remove(ctx_, ids_of(d.volatility_delta_surfaces));
    if (!d.volatility_strike_surfaces.empty())
        curve_volatility_strike_surface_repository().remove(ctx_, ids_of(d.volatility_strike_surfaces));
    if (!d.volatility_curves.empty())
        curve_volatility_curve_repository().remove(ctx_, ids_of(d.volatility_curves));
    if (!d.volatility_constants.empty())
        curve_volatility_constant_repository().remove(ctx_, ids_of(d.volatility_constants));
    if (!d.volatility_kinds.empty())
        curve_volatility_kind_repository().remove(ctx_, ids_of(d.volatility_kinds));
    if (!d.volatility_configs.empty())
        curve_volatility_config_repository().remove(ctx_, ids_of(d.volatility_configs));
    if (!d.cds_volatility_terms.empty())
        cds_volatility_term_repository().remove(ctx_, ids_of(d.cds_volatility_terms));
    if (!d.cds_volatilities.empty())
        cds_volatility_config_repository().remove(ctx_, ids_of(d.cds_volatilities));
    if (!d.report_configurations.empty())
        curve_report_configuration_repository().remove(ctx_, ids_of(d.report_configurations));
    if (!d.correlations.empty())
        curve_correlation_config_repository().remove(ctx_, ids_of(d.correlations));
    if (!d.base_correlations.empty())
        base_correlation_config_repository().remove(ctx_, ids_of(d.base_correlations));
    if (!d.fx_volatility_smiles.empty())
        fx_volatility_smile_repository().remove(ctx_, ids_of(d.fx_volatility_smiles));
    if (!d.fx_volatilities.empty())
        fx_volatility_config_repository().remove(ctx_, ids_of(d.fx_volatilities));
    if (!d.intraday_power_curves.empty())
        intraday_power_curve_config_repository().remove(ctx_, ids_of(d.intraday_power_curves));
    if (!d.securities.empty())
        curve_security_config_repository().remove(ctx_, ids_of(d.securities));
    if (!d.seasonality_factors.empty())
        inflation_seasonality_factor_repository().remove(ctx_, ids_of(d.seasonality_factors));
    if (!d.inflation_curves.empty())
        inflation_curve_config_repository().remove(ctx_, ids_of(d.inflation_curves));
    if (!d.default_curve_configurations.empty())
        default_curve_configuration_repository().remove(ctx_,
                                                        ids_of(d.default_curve_configurations));
    if (!d.default_curves.empty())
        default_curve_config_repository().remove(ctx_, ids_of(d.default_curves));
    if (!d.equity_curves.empty())
        equity_curve_config_repository().remove(ctx_, ids_of(d.equity_curves));
    if (!d.yield_curves.empty())
        yield_curve_config_repository().remove(ctx_, ids_of(d.yield_curves));
    if (!d.definitions.empty())
        curve_definition_repository().remove(ctx_, ids_of(d.definitions));
    if (!d.global_reports.empty())
        curve_global_report_repository().remove(ctx_, ids_of(d.global_reports));
    if (!d.sections.empty())
        curve_configuration_section_repository().remove(ctx_, ids_of(d.sections));
    curve_configuration_repository().remove(ctx_, boost::uuids::to_string(d.config.id));
}

std::optional<boost::uuids::uuid> curve_configuration_document_service::find_by_configuration(
    const boost::uuids::uuid& configuration_id) {
    for (const auto& h : curve_configuration_repository().read_latest(ctx_))
        if (h.configuration_id == configuration_id)
            return h.id;
    return std::nullopt;
}

}
