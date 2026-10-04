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
#ifndef ORES_REFDATA_API_DOMAIN_CONVENTIONS_DOCUMENT_HPP
#define ORES_REFDATA_API_DOMAIN_CONVENTIONS_DOCUMENT_HPP

#include "ores.refdata.api/domain/average_ois_convention.hpp"
#include "ores.refdata.api/domain/bma_basis_swap_convention.hpp"
#include "ores.refdata.api/domain/bond_yield_convention.hpp"
#include "ores.refdata.api/domain/cds_convention.hpp"
#include "ores.refdata.api/domain/cms_spread_option_convention.hpp"
#include "ores.refdata.api/domain/commodity_forward_convention.hpp"
#include "ores.refdata.api/domain/commodity_future_convention.hpp"
#include "ores.refdata.api/domain/cross_currency_basis_convention.hpp"
#include "ores.refdata.api/domain/cross_currency_fix_float_convention.hpp"
#include "ores.refdata.api/domain/currency_pair.hpp"
#include "ores.refdata.api/domain/currency_pair_convention.hpp"
#include "ores.refdata.api/domain/deposit_convention.hpp"
#include "ores.refdata.api/domain/fra_convention.hpp"
#include "ores.refdata.api/domain/future_convention.hpp"
#include "ores.refdata.api/domain/fx_option_convention.hpp"
#include "ores.refdata.api/domain/ibor_index_convention.hpp"
#include "ores.refdata.api/domain/inflation_swap_convention.hpp"
#include "ores.refdata.api/domain/intraday_power_load_convention.hpp"
#include "ores.refdata.api/domain/ois_convention.hpp"
#include "ores.refdata.api/domain/overnight_index_convention.hpp"
#include "ores.refdata.api/domain/swap_convention.hpp"
#include "ores.refdata.api/domain/swap_index_convention.hpp"
#include "ores.refdata.api/domain/tenor_basis_swap_convention.hpp"
#include "ores.refdata.api/domain/tenor_basis_two_swap_convention.hpp"
#include "ores.refdata.api/domain/zero_convention.hpp"
#include "ores.refdata.api/domain/zero_inflation_index_convention.hpp"
#include <string>
#include <vector>

namespace ores::refdata::domain {

/**
 * @brief An FX convention as a conventions document carries it: the pair, its
 * convention, and the advance calendars the document lists.
 */
struct fx_convention {
    currency_pair pair;
    currency_pair_convention convention;
    int spot_days = 0;
    std::vector<std::string> advance_calendars;

    friend bool operator==(const fx_convention&, const fx_convention&) = default;
};

/**
 * @brief One ORE conventions document as the rows refdata stores.
 *
 * The instrument conventions belong to a party. The index and FX conventions
 * are world data, which every party in the tenant shares.
 */
struct conventions_document {
    std::vector<zero_convention> zero;
    std::vector<average_ois_convention> average_ois;
    std::vector<bma_basis_swap_convention> bma_basis_swap;
    std::vector<cross_currency_basis_convention> cross_currency_basis;
    std::vector<cross_currency_fix_float_convention> cross_currency_fix_float;
    std::vector<tenor_basis_swap_convention> tenor_basis_swap;
    std::vector<tenor_basis_two_swap_convention> tenor_basis_two_swap;
    std::vector<deposit_convention> deposit;
    std::vector<swap_convention> swap;
    std::vector<swap_index_convention> swap_index;
    std::vector<future_convention> future;
    std::vector<fx_option_convention> fx_option;
    std::vector<inflation_swap_convention> inflation_swap;
    std::vector<intraday_power_load_convention> intraday_power_load;
    std::vector<ois_convention> ois;
    std::vector<fra_convention> fra;
    std::vector<ibor_index_convention> ibor_index;
    std::vector<overnight_index_convention> overnight_index;
    std::vector<zero_inflation_index_convention> zero_inflation_index;
    std::vector<fx_convention> fx;
    std::vector<cds_convention> cds;
    std::vector<cms_spread_option_convention> cms_spread_option;
    std::vector<commodity_future_convention> commodity_future;
    std::vector<commodity_forward_convention> commodity_forward;
    std::vector<bond_yield_convention> bond_yield;

    friend bool operator==(const conventions_document&, const conventions_document&) = default;
};

}

#endif
