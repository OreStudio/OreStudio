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
#ifndef ORES_ORE_CORE_DOMAIN_CREDIT_INSTRUMENT_MAPPER_HPP
#define ORES_ORE_CORE_DOMAIN_CREDIT_INSTRUMENT_MAPPER_HPP

#include "ores.logging/make_logger.hpp"
#include "ores.ore.core/domain/domain.hpp"
#include "ores.ore.core/export.hpp"
#include "ores.trading.api/domain/credit_instrument.hpp"
#include "ores.trading.api/domain/instrument.hpp"

namespace ores::ore::domain {

/**
 * @brief Maps ORE XSD credit trade types to ORES domain types and back.
 *
 * Handles:
 *   - CreditDefaultSwap       (CreditDefaultSwapData)
 *   - IndexCreditDefaultSwap  (IndexCreditDefaultSwapData)
 *   - IndexCreditDefaultSwapOption (IndexCreditDefaultSwapOptionData)
 *   - CreditLinkedSwap        (CreditLinkedSwapData)
 *   - SyntheticCDO            (CdoData)
 *   - RiskParticipationAgreement (RiskParticipationAgreementData)
 */
class ORES_ORE_CORE_EXPORT credit_instrument_mapper {
private:
    inline static std::string_view logger_name = "ores.ore.domain.credit_instrument_mapper";

    static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    /**
     * The credit trade as the family carrier: the header, the product's fact
     * row, the trade's legs as rows of the shared leg tables, the names of a
     * basket, and an index option's strike and exercise dates as shared strike
     * and schedule rows. A product that states none of them leaves the
     * collection empty.
     */
    static trading::domain::credit_instrument_data forward_cds(const trade& t);
    static trading::domain::credit_instrument_data forward_index_cds(const trade& t);
    static trading::domain::credit_instrument_data forward_index_cds_option(const trade& t);
    static trading::domain::credit_instrument_data forward_credit_linked_swap(const trade& t);
    static trading::domain::credit_instrument_data forward_synthetic_cdo(const trade& t);
    static trading::domain::credit_instrument_data forward_rpa(const trade& t);

    static trade reverse_cds(const ores::trading::domain::credit_instrument_data& data);
    static trade reverse_index_cds(const ores::trading::domain::credit_instrument_data& data);
    static trade reverse_index_cds_option(const ores::trading::domain::credit_instrument_data& data);
    static trade
    reverse_credit_linked_swap(const ores::trading::domain::credit_instrument_data& data);
    static trade reverse_synthetic_cdo(const ores::trading::domain::credit_instrument_data& data);
    static trade reverse_rpa(const ores::trading::domain::credit_instrument_data& data);

private:
    /** Appends one ORE leg to the carrier as rows of the shared leg tables. */
    static void append_leg(ores::trading::domain::credit_instrument_data& result,
                           const legData& ld,
                           const std::string& group);

    /** Appends the names of an ORE basket as constituent rows. */
    static void append_basket(ores::trading::domain::credit_instrument_data& result,
                              const basketData& basket);

    /** The ORE legs of one group, rebuilt from the leg rows in leg order. */
    static std::vector<legData>
    reverse_group(const ores::trading::domain::credit_instrument_data& data,
                  const std::string& group);

    /** The ORE basket rebuilt from the constituent rows. */
    static basketData
    reverse_basket(const std::vector<ores::trading::domain::credit_basket_constituent>& names);
};

}

#endif
