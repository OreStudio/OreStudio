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
/**
 * AUTO-GENERATED FILE - DO NOT EDIT MANUALLY
 * Template: cpp_convention_family_units.hpp.mustache
 * To modify, update the template and regenerate.
 */
#ifndef ORES_REFDATA_CORE_SERVICE_CONVENTION_FAMILY_UNITS_HPP
#define ORES_REFDATA_CORE_SERVICE_CONVENTION_FAMILY_UNITS_HPP

#include "ores.database/domain/context.hpp"
#include "ores.refdata.core/service/conventions_bundle.hpp"
#include <string>
#include <vector>

namespace ores::refdata::service::convention_family {

// One pair per convention family. Each pair is defined in a translation unit
// of its own, so no unit instantiates the serialisation of two families.

void apply_average_ois(const ores::database::context& ctx,
                       const std::string& payload,
                       conventions_apply_result& result);

std::vector<std::string> read_average_ois_payloads(const ores::database::context& ctx);

void apply_bma_basis_swap(const ores::database::context& ctx,
                          const std::string& payload,
                          conventions_apply_result& result);

std::vector<std::string> read_bma_basis_swap_payloads(const ores::database::context& ctx);

void apply_bond_yield(const ores::database::context& ctx,
                      const std::string& payload,
                      conventions_apply_result& result);

std::vector<std::string> read_bond_yield_payloads(const ores::database::context& ctx);

void apply_cds(const ores::database::context& ctx,
               const std::string& payload,
               conventions_apply_result& result);

std::vector<std::string> read_cds_payloads(const ores::database::context& ctx);

void apply_cms_spread_option(const ores::database::context& ctx,
                             const std::string& payload,
                             conventions_apply_result& result);

std::vector<std::string> read_cms_spread_option_payloads(const ores::database::context& ctx);

void apply_commodity_forward(const ores::database::context& ctx,
                             const std::string& payload,
                             conventions_apply_result& result);

std::vector<std::string> read_commodity_forward_payloads(const ores::database::context& ctx);

void apply_commodity_future(const ores::database::context& ctx,
                            const std::string& payload,
                            conventions_apply_result& result);

std::vector<std::string> read_commodity_future_payloads(const ores::database::context& ctx);

void apply_convention(const ores::database::context& ctx,
                      const std::string& payload,
                      conventions_apply_result& result);

std::vector<std::string> read_convention_payloads(const ores::database::context& ctx);

void apply_cross_currency_basis(const ores::database::context& ctx,
                                const std::string& payload,
                                conventions_apply_result& result);

std::vector<std::string> read_cross_currency_basis_payloads(const ores::database::context& ctx);

void apply_cross_currency_fix_float(const ores::database::context& ctx,
                                    const std::string& payload,
                                    conventions_apply_result& result);

std::vector<std::string> read_cross_currency_fix_float_payloads(const ores::database::context& ctx);

void apply_deposit(const ores::database::context& ctx,
                   const std::string& payload,
                   conventions_apply_result& result);

std::vector<std::string> read_deposit_payloads(const ores::database::context& ctx);

void apply_fra(const ores::database::context& ctx,
               const std::string& payload,
               conventions_apply_result& result);

std::vector<std::string> read_fra_payloads(const ores::database::context& ctx);

void apply_future(const ores::database::context& ctx,
                  const std::string& payload,
                  conventions_apply_result& result);

std::vector<std::string> read_future_payloads(const ores::database::context& ctx);

void apply_fx_option(const ores::database::context& ctx,
                     const std::string& payload,
                     conventions_apply_result& result);

std::vector<std::string> read_fx_option_payloads(const ores::database::context& ctx);

void apply_ibor_index(const ores::database::context& ctx,
                      const std::string& payload,
                      conventions_apply_result& result);

std::vector<std::string> read_ibor_index_payloads(const ores::database::context& ctx);

void apply_inflation_swap(const ores::database::context& ctx,
                          const std::string& payload,
                          conventions_apply_result& result);

std::vector<std::string> read_inflation_swap_payloads(const ores::database::context& ctx);

void apply_intraday_power_load(const ores::database::context& ctx,
                               const std::string& payload,
                               conventions_apply_result& result);

std::vector<std::string> read_intraday_power_load_payloads(const ores::database::context& ctx);

void apply_ois(const ores::database::context& ctx,
               const std::string& payload,
               conventions_apply_result& result);

std::vector<std::string> read_ois_payloads(const ores::database::context& ctx);

void apply_overnight_index(const ores::database::context& ctx,
                           const std::string& payload,
                           conventions_apply_result& result);

std::vector<std::string> read_overnight_index_payloads(const ores::database::context& ctx);

void apply_swap(const ores::database::context& ctx,
                const std::string& payload,
                conventions_apply_result& result);

std::vector<std::string> read_swap_payloads(const ores::database::context& ctx);

void apply_swap_index(const ores::database::context& ctx,
                      const std::string& payload,
                      conventions_apply_result& result);

std::vector<std::string> read_swap_index_payloads(const ores::database::context& ctx);

void apply_tenor_basis_swap_long_short(const ores::database::context& ctx,
                                       const std::string& payload,
                                       conventions_apply_result& result);

std::vector<std::string>
read_tenor_basis_swap_long_short_payloads(const ores::database::context& ctx);

void apply_tenor_basis_swap_pay_receive(const ores::database::context& ctx,
                                        const std::string& payload,
                                        conventions_apply_result& result);

std::vector<std::string>
read_tenor_basis_swap_pay_receive_payloads(const ores::database::context& ctx);

void apply_tenor_basis_two_swap(const ores::database::context& ctx,
                                const std::string& payload,
                                conventions_apply_result& result);

std::vector<std::string> read_tenor_basis_two_swap_payloads(const ores::database::context& ctx);

void apply_zero(const ores::database::context& ctx,
                const std::string& payload,
                conventions_apply_result& result);

std::vector<std::string> read_zero_payloads(const ores::database::context& ctx);

void apply_zero_inflation_index(const ores::database::context& ctx,
                                const std::string& payload,
                                conventions_apply_result& result);

std::vector<std::string> read_zero_inflation_index_payloads(const ores::database::context& ctx);

}

#endif
