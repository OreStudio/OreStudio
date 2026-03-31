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
#ifndef ORES_ORE_DOMAIN_CREDIT_INSTRUMENT_MAPPER_HPP
#define ORES_ORE_DOMAIN_CREDIT_INSTRUMENT_MAPPER_HPP

#include "ores.logging/make_logger.hpp"
#include "ores.ore/domain/domain.hpp"
#include "ores.trading.api/domain/credit_instrument.hpp"

namespace ores::ore::domain {

/**
 * @brief Result of a forward mapping from ORE XSD to the ORES credit domain type.
 */
struct credit_mapping_result {
    ores::trading::domain::credit_instrument instrument;
};

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
class credit_instrument_mapper {
private:
    inline static std::string_view logger_name =
        "ores.ore.domain.credit_instrument_mapper";

    static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    static credit_mapping_result forward_cds(const trade& t);
    static credit_mapping_result forward_index_cds(const trade& t);
    static credit_mapping_result forward_index_cds_option(const trade& t);
    static credit_mapping_result forward_credit_linked_swap(const trade& t);
    static credit_mapping_result forward_synthetic_cdo(const trade& t);
    static credit_mapping_result forward_rpa(const trade& t);

    static trade reverse_cds(const ores::trading::domain::credit_instrument& instr);
    static trade reverse_index_cds(
        const ores::trading::domain::credit_instrument& instr);
    static trade reverse_index_cds_option(
        const ores::trading::domain::credit_instrument& instr);
    static trade reverse_credit_linked_swap(
        const ores::trading::domain::credit_instrument& instr);
    static trade reverse_synthetic_cdo(
        const ores::trading::domain::credit_instrument& instr);
    static trade reverse_rpa(
        const ores::trading::domain::credit_instrument& instr);

private:
    static void map_cds_leg(
        const legData& ld,
        ores::trading::domain::credit_instrument& instr);

    static legData reverse_cds_leg(
        const ores::trading::domain::credit_instrument& instr);
};

}

#endif
