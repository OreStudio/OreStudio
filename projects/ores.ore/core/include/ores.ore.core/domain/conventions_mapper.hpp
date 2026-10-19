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
#include "ores.ore.core/domain/conventions_package.hpp"
#include "ores.ore.core/export.hpp"
#include "ores.ore.core/xml/record.hpp"
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
#include "ores.refdata.api/domain/tenor_basis_two_swap_convention.hpp"
#include "ores.refdata.api/domain/zero_convention.hpp"
#include "ores.refdata.api/domain/zero_inflation_index_convention.hpp"
#include "ores.refdata.api/messaging/configuration_document_protocol.hpp"
#include "ores.refdata.api/messaging/tenor_basis_swap_convention_protocol.hpp"
#include <cstddef>
#include <map>
#include <string>
#include <string_view>
#include <vector>

namespace ores::ore::domain {

/**
 * @brief Maps between the codec's convention records and refdata domain types.
 *
 * The mapper normalises all ORE enum aliases (e.g. "F", "Following",
 * "FOLLOWING") to canonical FpML/CDM codes before storing them in the domain
 * types. Each family has a @c map_ method that reads one element and a
 * @c reverse_ method that writes one back. Both are defined in the family's
 * own unit and are exposed for unit testing.
 */
class ORES_ORE_CORE_EXPORT conventions_mapper {
private:
    inline static std::string_view logger_name = "ores.ore.domain.conventions_mapper";

    static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    /**
     * @brief Reads one ORE conventions document into the codec's record.
     *
     * @param origin Names the document in the refusal, a file or a service.
     * @throws std::invalid_argument when the text is not a conventions document.
     */
    static conventions read(std::string_view text, std::string_view origin = {});

    /// Writes the record as an ORE conventions document.
    static std::string write(const conventions& v);

    /**
     * @brief The categories the document carries that no entity models, and how
     * many elements each holds.
     *
     * A skipped category that is counted is a gap a caller can act on. A silent
     * skip is a document that lost content and said nothing, which is how this
     * kind went unmeasured for as long as it did: seventy-two files passed a
     * round-trip test that only compared element counts.
     */
    static std::map<std::string, std::size_t> unmodelled(const conventions& v);

    static refdata::domain::average_ois_convention map_average_ois(const xml::record_reader& v);

    static void reverse_average_ois(const refdata::domain::average_ois_convention& v,
                                    xml::record_writer w);

    static refdata::domain::bma_basis_swap_convention
    map_bma_basis_swap(const xml::record_reader& v);

    static void reverse_bma_basis_swap(const refdata::domain::bma_basis_swap_convention& v,
                                       xml::record_writer w);

    static refdata::domain::bond_yield_convention map_bond_yield(const xml::record_reader& v);

    static void reverse_bond_yield(const refdata::domain::bond_yield_convention& v,
                                   xml::record_writer w);

    static refdata::domain::cds_convention map_cds(const xml::record_reader& v);

    static void reverse_cds(const refdata::domain::cds_convention& v, xml::record_writer w);

    static refdata::domain::cms_spread_option_convention
    map_cms_spread_option(const xml::record_reader& v);

    static void reverse_cms_spread_option(const refdata::domain::cms_spread_option_convention& v,
                                          xml::record_writer w);

    static refdata::domain::commodity_forward_convention
    map_commodity_forward(const xml::record_reader& v);

    static void reverse_commodity_forward(const refdata::domain::commodity_forward_convention& v,
                                          xml::record_writer w);

    static refdata::domain::commodity_future_convention
    map_commodity_future(const xml::record_reader& v);

    static void reverse_commodity_future(const refdata::domain::commodity_future_convention& v,
                                         xml::record_writer w);

    static refdata::domain::cross_currency_basis_convention
    map_cross_currency_basis(const xml::record_reader& v);

    static void
    reverse_cross_currency_basis(const refdata::domain::cross_currency_basis_convention& v,
                                 xml::record_writer w);

    static refdata::domain::cross_currency_fix_float_convention
    map_cross_currency_fix_float(const xml::record_reader& v);

    static void
    reverse_cross_currency_fix_float(const refdata::domain::cross_currency_fix_float_convention& v,
                                     xml::record_writer w);

    static refdata::domain::deposit_convention map_deposit(const xml::record_reader& v);

    static void reverse_deposit(const refdata::domain::deposit_convention& v, xml::record_writer w);

    static refdata::domain::fra_convention map_fra(const xml::record_reader& v);

    static void reverse_fra(const refdata::domain::fra_convention& v, xml::record_writer w);

    static refdata::domain::future_convention map_future(const xml::record_reader& v);

    static void reverse_future(const refdata::domain::future_convention& v, xml::record_writer w);

    static ores::refdata::messaging::fx_convention map_fx(const xml::record_reader& v);

    static void reverse_fx(const ores::refdata::messaging::fx_convention& v, xml::record_writer w);

    static refdata::domain::fx_option_convention map_fx_option(const xml::record_reader& v);

    static void reverse_fx_option(const refdata::domain::fx_option_convention& v,
                                  xml::record_writer w);

    static refdata::domain::ibor_index_convention map_ibor_index(const xml::record_reader& v);

    static void reverse_ibor_index(const refdata::domain::ibor_index_convention& v,
                                   xml::record_writer w);

    static refdata::domain::inflation_swap_convention
    map_inflation_swap(const xml::record_reader& v);

    static void reverse_inflation_swap(const refdata::domain::inflation_swap_convention& v,
                                       xml::record_writer w);

    static refdata::domain::intraday_power_load_convention
    map_intraday_power_load(const xml::record_reader& v);

    static void
    reverse_intraday_power_load(const refdata::domain::intraday_power_load_convention& v,
                                xml::record_writer w);

    static refdata::domain::ois_convention map_ois(const xml::record_reader& v);

    static void reverse_ois(const refdata::domain::ois_convention& v, xml::record_writer w);

    static refdata::domain::overnight_index_convention
    map_overnight_index(const xml::record_reader& v);

    static void reverse_overnight_index(const refdata::domain::overnight_index_convention& v,
                                        xml::record_writer w);

    static refdata::domain::swap_convention map_swap(const xml::record_reader& v);

    static void reverse_swap(const refdata::domain::swap_convention& v, xml::record_writer w);

    static refdata::domain::swap_index_convention map_swap_index(const xml::record_reader& v);

    static void reverse_swap_index(const refdata::domain::swap_index_convention& v,
                                   xml::record_writer w);

    static ores::refdata::messaging::tenor_basis_swap_convention_item
    map_tenor_basis_swap(const xml::record_reader& v);

    static void
    reverse_tenor_basis_swap(const ores::refdata::messaging::tenor_basis_swap_convention_item& v,
                             xml::record_writer w);

    static refdata::domain::tenor_basis_two_swap_convention
    map_tenor_basis_two_swap(const xml::record_reader& v);

    static void
    reverse_tenor_basis_two_swap(const refdata::domain::tenor_basis_two_swap_convention& v,
                                 xml::record_writer w);

    static refdata::domain::zero_convention map_zero(const xml::record_reader& v);

    static void reverse_zero(const refdata::domain::zero_convention& v, xml::record_writer w);

    static refdata::domain::zero_inflation_index_convention
    map_zero_inflation_index(const xml::record_reader& v);

    static void
    reverse_zero_inflation_index(const refdata::domain::zero_inflation_index_convention& v,
                                 xml::record_writer w);
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
