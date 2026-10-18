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
 * FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License along with
 * this program; if not, write to the Free Software Foundation, Inc., 51
 * Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
 */
#ifndef ORES_ORE_CORE_DOMAIN_RUN_DOCUMENT_PROJECTION_HPP
#define ORES_ORE_CORE_DOMAIN_RUN_DOCUMENT_PROJECTION_HPP

#include "ores.ore.core/export.hpp"
#include "ores.ore.core/xml/generated/ore.hpp"
#include "ores.ore.core/xml/record.hpp"
#include "ores.reporting.api/domain/configuration_market_binding.hpp"
#include "ores.reporting.api/domain/risk_report_config.hpp"
#include "ores.reporting.api/messaging/run_document_protocol.hpp"
#include <string>
#include <string_view>
#include <vector>

namespace ores::ore::domain {

/**
 * @brief What the Setup block of a run document holds.
 *
 * The choices belong to the configuration and are stored. The names of the files
 * the run reads are data, not choices, and are not stored: a configuration does
 * not name its files. They travel beside the choices so an import can find the
 * files and an export can write them back.
 */
struct mapped_setup final {
    reporting::domain::risk_report_config settings;
    std::vector<ores::reporting::messaging::run_file> files;
};

/**
 * @brief Projects an ORE run document onto the reporting configuration entities,
 * and back.
 *
 * The Setup block is the first half. Each parameter that is a choice of the run
 * is read into its column of risk_report_config. Each parameter that names a file
 * is read into a run_file. The projection writes both back as the named
 * parameters ORE expects.
 *
 * Values are carried as ORE spells them. The flags are not all booleans -- the
 * fixing and cashflow flags are Y or N, the model flags are true or false, and
 * accrualDate may be the literal ASOF -- so the columns are text and the mapping
 * is a rename, not a conversion.
 *
 * The as-of date is the one parameter with a column pair: asofDate is the
 * market_data_type 'date' together with market_data_date.
 */
class ORES_ORE_CORE_EXPORT run_document_projection {
public:
    using document = xml::generated::ore::ore;

    /**
     * @brief Projects a run document onto the entities.
     *
     * A Setup parameter that is neither a choice nor a file is refused rather
     * than dropped, and a value that cannot be read as the column's type is
     * refused too.
     */
    static ores::reporting::messaging::run_document map(const document& v);

    /**
     * @brief Reconstructs a run document from the entities.
     *
     * The Setup parameters are written in a fixed order, which is not
     * necessarily the order the document wrote them in: the entity is a row of
     * columns and has no order of its own. ORE reads the parameters by name, so
     * the order is lossy on purpose.
     */
    static document reverse(const ores::reporting::messaging::run_document& v);

    static mapped_setup map_setup(const document& v);
    static std::vector<ores::reporting::messaging::run_analytic> map_analytics(const document& v);
    static std::vector<reporting::domain::configuration_market_binding>
    map_market_bindings(const document& v);

    static void reverse_setup(xml::record_writer out, const mapped_setup& v);
    static void reverse_analytics(xml::record_writer out,
                                  const std::vector<ores::reporting::messaging::run_analytic>& v);
    static void
    reverse_market_bindings(xml::record_writer out,
                            const std::vector<reporting::domain::configuration_market_binding>& v);

    static document read(std::string_view text, std::string_view origin = {});
    static std::string write(const document& v);
};

}

#endif
