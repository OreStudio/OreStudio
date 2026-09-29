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
#ifndef ORES_ORE_CORE_DOMAIN_CONVENTIONS_MAPPER_HPP
#define ORES_ORE_CORE_DOMAIN_CONVENTIONS_MAPPER_HPP

#include "ores.logging/make_logger.hpp"
#include "ores.ore.core/domain/domain.hpp"
#include "ores.ore.core/export.hpp"
#include "ores.refdata.api/domain/average_ois_convention.hpp"
#include "ores.refdata.api/domain/bma_basis_swap_convention.hpp"
#include "ores.refdata.api/domain/bond_yield_convention.hpp"
#include "ores.refdata.api/domain/commodity_forward_convention.hpp"
#include "ores.refdata.api/domain/cds_convention.hpp"
#include "ores.refdata.api/domain/cms_spread_option_convention.hpp"
#include "ores.refdata.api/domain/commodity_future_convention.hpp"
#include "ores.refdata.api/domain/currency_pair.hpp"
#include "ores.refdata.api/domain/cross_currency_basis_convention.hpp"
#include "ores.refdata.api/domain/cross_currency_fix_float_convention.hpp"
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
#include "ores.refdata.api/domain/zero_inflation_index_convention.hpp"
#include "ores.refdata.api/domain/tenor_basis_two_swap_convention.hpp"
#include "ores.refdata.api/domain/zero_convention.hpp"
#include <cstddef>
#include <map>
#include <string>
#include <vector>

namespace ores::ore::domain {

/**
 * @brief A currency_pair identity paired with its 1:1 convention record, as produced by mapping a
 * single ORE XML <FX> element.
 *
 * @c spot_days and @c advance_calendars are carried here rather than on @c currency_pair or
 * @c currency_pair_convention: neither is persisted on either domain type (spot days is derived
 * from the two legs' currency.spot_days at read time; advance calendars live in the
 * currency_pair_convention_calendar junction, not a column), but this mapper has no database
 * access and must still round-trip both values byte-for-byte between ORE XML import and export.
 * @c advance_calendars holds the individual calendar codes ORE's comma-joined
 * <AdvanceCalendar> element names (e.g. {"TARGET", "UnitedKingdom"}); building/parsing the
 * comma-joined string itself, and resolving it against the junction table, is the caller's
 * responsibility.
 */
struct mapped_fx {
    refdata::domain::currency_pair pair;
    refdata::domain::currency_pair_convention convention;
    int spot_days = 0;
    std::vector<std::string> advance_calendars;

    friend bool operator==(const mapped_fx&, const mapped_fx&) = default;
};

/**
 * @brief All convention types extracted from a single ORE conventions.xml file.
 *
 * The nine fields correspond to the nine convention categories currently
 * modelled in the ORES refdata domain. ORE conventions.xml contains additional
 * types (AverageOIS, TenorBasisSwap, CrossCurrencyBasis, InflationSwap, etc.)
 * that are not yet modelled and are silently skipped during import.
 */
struct mapped_conventions {
    std::vector<refdata::domain::zero_convention> zero;
    std::vector<refdata::domain::average_ois_convention> average_ois;
    std::vector<refdata::domain::bma_basis_swap_convention> bma_basis_swap;
    std::vector<refdata::domain::cross_currency_basis_convention> cross_currency_basis;
    std::vector<refdata::domain::cross_currency_fix_float_convention> cross_currency_fix_float;
    std::vector<refdata::domain::tenor_basis_swap_convention> tenor_basis_swap;
    std::vector<refdata::domain::tenor_basis_two_swap_convention> tenor_basis_two_swap;
    std::vector<refdata::domain::deposit_convention> deposit;
    std::vector<refdata::domain::swap_convention> swap;
    std::vector<refdata::domain::swap_index_convention> swap_index;
    std::vector<refdata::domain::future_convention> future;
    std::vector<refdata::domain::fx_option_convention> fx_option;
    std::vector<refdata::domain::inflation_swap_convention> inflation_swap;
    std::vector<refdata::domain::intraday_power_load_convention> intraday_power_load;
    std::vector<refdata::domain::ois_convention> ois;
    std::vector<refdata::domain::fra_convention> fra;
    std::vector<refdata::domain::ibor_index_convention> ibor_index;
    std::vector<refdata::domain::overnight_index_convention> overnight_index;
    std::vector<refdata::domain::zero_inflation_index_convention> zero_inflation_index;
    std::vector<mapped_fx> fx;
    std::vector<refdata::domain::cds_convention> cds;
    std::vector<refdata::domain::cms_spread_option_convention> cms_spread_option;
    std::vector<refdata::domain::commodity_future_convention> commodity_future;
    std::vector<refdata::domain::commodity_forward_convention> commodity_forward;
    std::vector<refdata::domain::bond_yield_convention> bond_yield;

    /**
     * @brief The categories the mapper read but does not model, and how many
     * elements each held.
     *
     * A skipped category that is counted is a gap a caller can act on. A silent
     * skip is a document that lost content and said nothing, which is how this
     * kind went unmeasured for as long as it did: seventy-two files passed a
     * round-trip test that only compared element counts.
     */
    std::map<std::string, std::size_t> unmodelled;

    friend bool operator==(const mapped_conventions&, const mapped_conventions&) = default;
};

/**
 * @brief Maps between ORE XML convention types and refdata domain types.
 *
 * The mapper normalises all ORE enum aliases (e.g. "F", "Following",
 * "FOLLOWING") to canonical FpML/CDM codes before storing them in the domain
 * types. Individual @c map_* methods are exposed for unit testing.
 */
class ORES_ORE_CORE_EXPORT conventions_mapper {
private:
    inline static std::string_view logger_name = "ores.ore.domain.conventions_mapper";

    static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

    // Normalisation helpers — collapse ORE aliases to canonical codes.
    static std::string normalize_bdc(domain::businessDayConvention v);
    static std::string normalize_day_counter(domain::dayCounter v);
    static std::string normalize_frequency(domain::frequencyType v);
    static std::string normalize_compounding(domain::compounding v);
    static std::string normalize_date_rule(domain::dateRule v);

    static bool parse_bool(domain::bool_ v);

public:
    /**
     * @brief Maps all recognised convention types from an ORE conventions doc.
     */
    static mapped_conventions map(const conventions& v);

    static refdata::domain::zero_convention map_zero(const zeroType& v);

    static refdata::domain::deposit_convention map_deposit(const depositType& v);

    static refdata::domain::swap_convention map_swap(const swapType& v);

    static refdata::domain::swap_index_convention map_swap_index(const swapIndexType& v);

    static refdata::domain::future_convention map_future(const futureType& v);

    static refdata::domain::fx_option_convention map_fx_option(const fxOption& v);

    static refdata::domain::average_ois_convention map_average_ois(const averageOISType& v);

    static refdata::domain::cross_currency_basis_convention
    map_cross_currency_basis(const crossCurrencyBasisType& v);

    static refdata::domain::cross_currency_fix_float_convention
    map_cross_currency_fix_float(const crossCurrencyFixFloatType& v);

    static refdata::domain::tenor_basis_swap_convention
    map_tenor_basis_swap(const tenorBasisSwapType& v);

    static refdata::domain::tenor_basis_two_swap_convention
    map_tenor_basis_two_swap(const tenorBasisTwoSwapType& v);

    static refdata::domain::ois_convention map_ois(const oisType& v);

    static refdata::domain::fra_convention map_fra(const fraType& v);

    static refdata::domain::ibor_index_convention map_ibor_index(const iborIndexType& v);

    static refdata::domain::inflation_swap_convention
    map_inflation_swap(const inflationswapType& v);

    static refdata::domain::intraday_power_load_convention
    map_intraday_power_load(const intradayPowerLoad& v);

    static refdata::domain::overnight_index_convention
    map_overnight_index(const overnightIndexType& v);

    static refdata::domain::zero_inflation_index_convention
    map_zero_inflation_index(const zeroInflationIndexType& v);

    static refdata::domain::bma_basis_swap_convention
    map_bma_basis_swap(const bmaBasisSwapType& v);

    static mapped_fx map_fx(const fxType& v);

    static refdata::domain::cds_convention map_cds(const cdsConventionsType& v);

    static refdata::domain::cms_spread_option_convention
    map_cms_spread_option(const cmsSpreadOptionType& v);

    static refdata::domain::commodity_future_convention
    map_commodity_future(const commodityFutureType& v);

    static refdata::domain::commodity_forward_convention
    map_commodity_forward(const commodityForwardType& v);

    static refdata::domain::bond_yield_convention map_bond_yield(const bondYield& v);

    /**
     * @brief Reconstructs an ORE conventions XML document from mapped domain conventions.
     */
    static domain::conventions reverse(const mapped_conventions& v);
};

/**
 * @brief The first difference between an imported and an exported document, or
 * empty when the two agree.
 *
 * Conventions are the case where a plain text comparison cannot work. The mapper
 * collapses ORE's boolean spellings and its enum aliases to the canonical codes
 * the refdata columns hold, so =true= comes back as =True= and =A365= as =A365F=
 * on a document that lost nothing. Only this component can tell that
 * normalisation from a loss, so the comparison lives beside the mappers.
 *
 * It refuses an element the export writes fewer times than the document did, and
 * one the export invents, and then requires the same mapper to read the same
 * conventions out of both documents.
 *
 * @param path Prefixed to the message, so a caller walking a corpus can say
 * which file disagreed
 */
ORES_ORE_CORE_EXPORT std::string conventions_difference(const conventions& original,
                                                        const conventions& exported,
                                                        const std::string& path);

}

#endif
