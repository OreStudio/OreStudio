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
#ifndef ORES_ORE_CORE_DOMAIN_BOND_INSTRUMENT_MAPPER_HPP
#define ORES_ORE_CORE_DOMAIN_BOND_INSTRUMENT_MAPPER_HPP

#include "ores.logging/make_logger.hpp"
#include "ores.ore.core/domain/domain.hpp"
#include "ores.ore.core/export.hpp"
#include "ores.trading.api/domain/bond_instrument_data.hpp"
#include <functional>
#include <optional>
#include <string>

namespace ores::ore::domain {

/**
 * @brief Finds the issue row a security identifier already has.
 *
 * The mapper is pure: it cannot reach a database. The import path
 * supplies this lookup so that a document whose security already has
 * an issue row points at that row instead of minting a second one.
 * An empty lookup mints a fresh issue for every mapped trade.
 */
using bond_issue_lookup = std::function<std::optional<ores::trading::domain::bond_issue>(
    const std::string& security_id)>;

/**
 * @brief Maps ORE XSD bond trade types to ORES domain rows and back.
 *
 * One mapper per entity: the issue row and its call-date and
 * conversion-target children, the instrument header, and a fact row
 * per product. The forward_* and reverse_* entry points assemble those
 * rows into a bond_instrument_data and join them back into the trade
 * document.
 *
 * Forward mapping retargets the storage the flattening mapper filled
 * into the wide legacy struct: the bondData fields land in the issue
 * row, the per-product economics in the fact row of the product, the
 * convertible's ratios and the callable's dates in the issue's child
 * rows, and what no row carries yet in the container remainder.
 * Reverse reconstruction joins the rows back and emits what the
 * flattening mapper emitted for each product.
 */
class ORES_ORE_CORE_EXPORT bond_instrument_mapper {
private:
    inline static std::string_view logger_name = "ores.ore.domain.bond_instrument_mapper";

    static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

    static void map_bond_data(const bondData& bd,
                              ores::trading::domain::bond_instrument_data& data);

    static bondData reverse_bond_data(const ores::trading::domain::bond_instrument_data& data);

    static void map_call_dates(const callableBondCallData& call_data,
                               boost::uuids::uuid issue_id,
                               std::vector<ores::trading::domain::bond_issue_call_date>& dates);

    static void reverse_call_dates(const std::vector<ores::trading::domain::bond_issue_call_date>& dates,
                                   callableBondCallData& call_data);

    static void map_conversion_targets(
        const cbConversionData& conversion_data,
        boost::uuids::uuid issue_id,
        std::vector<ores::trading::domain::bond_issue_conversion_target>& targets);

    static void reverse_conversion_targets(
        const std::vector<ores::trading::domain::bond_issue_conversion_target>& targets,
        cbConversionData& conversion_data);

public:
    static trading::domain::bond_instrument_data forward_bond(
        const trade& t, const bond_issue_lookup& lookup = {});
    static trading::domain::bond_instrument_data forward_forward_bond(
        const trade& t, const bond_issue_lookup& lookup = {});
    static trading::domain::bond_instrument_data forward_callable_bond(
        const trade& t, const bond_issue_lookup& lookup = {});
    static trading::domain::bond_instrument_data forward_convertible_bond(
        const trade& t, const bond_issue_lookup& lookup = {});
    static trading::domain::bond_instrument_data forward_bond_option(
        const trade& t, const bond_issue_lookup& lookup = {});
    static trading::domain::bond_instrument_data forward_bond_trs(
        const trade& t, const bond_issue_lookup& lookup = {});
    static trading::domain::bond_instrument_data forward_bond_repo(
        const trade& t, const bond_issue_lookup& lookup = {});
    static trading::domain::bond_instrument_data forward_bond_future(
        const trade& t, const bond_issue_lookup& lookup = {});
    static trading::domain::bond_instrument_data forward_ascot(
        const trade& t, const bond_issue_lookup& lookup = {});

    static trade reverse_bond(const trading::domain::bond_instrument_data& data);
    static trade reverse_forward_bond(const trading::domain::bond_instrument_data& data);
    static trade reverse_callable_bond(const trading::domain::bond_instrument_data& data);
    static trade reverse_convertible_bond(const trading::domain::bond_instrument_data& data);
    static trade reverse_bond_option(const trading::domain::bond_instrument_data& data);
    static trade reverse_bond_trs(const trading::domain::bond_instrument_data& data);
    static trade reverse_bond_repo(const trading::domain::bond_instrument_data& data);
    static trade reverse_bond_future(const trading::domain::bond_instrument_data& data);
    static trade reverse_ascot(const trading::domain::bond_instrument_data& data);
};

}

#endif
