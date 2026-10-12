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
#ifndef ORES_TRADING_DOMAIN_CREDIT_FACT_VARIANT_HPP
#define ORES_TRADING_DOMAIN_CREDIT_FACT_VARIANT_HPP

#include "ores.trading.api/domain/credit_default_swap_instrument.hpp"
#include "ores.trading.api/domain/credit_linked_swap_instrument.hpp"
#include "ores.trading.api/domain/index_credit_default_swap_instrument.hpp"
#include "ores.trading.api/domain/index_credit_default_swap_option_instrument.hpp"
#include "ores.trading.api/domain/risk_participation_agreement_instrument.hpp"
#include "ores.trading.api/domain/synthetic_cdo_instrument.hpp"
#include <variant>

namespace ores::trading::domain {

/**
 * @brief The credit family's fact rows, one alternative per product table.
 *
 * The alternatives carry the product's own fields only: the identity, the
 * reference entity, the dates and the description belong to the family
 * header, ores.trading.credit_instrument, and join by the trade id the fact
 * row carries flat. The family's legs, leg amounts and rates, its basket and
 * its strikes and schedules sit beside this variant in credit_instrument_data.
 */
using credit_fact_variant = std::variant<credit_default_swap_instrument,
                                         index_credit_default_swap_instrument,
                                         index_credit_default_swap_option_instrument,
                                         synthetic_cdo_instrument,
                                         credit_linked_swap_instrument,
                                         risk_participation_agreement_instrument>;

} // namespace ores::trading::domain

#endif
