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
#include <optional>
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
    std::optional<ores::reporting::messaging::run_engine_settings> engine_settings;
    std::vector<ores::reporting::messaging::run_file> files;

    /**
     * @brief The Setup parameters the model holds nowhere, by name.
     *
     * The projection drops them and the import reports each one.
     */
    std::vector<std::string> dropped;
};

/**
 * @brief Projects an ORE run document onto the reporting configuration entities,
 * and back.
 *
 * The Setup block is the first half. Each parameter that is a choice of the run
 * is read into its column of risk_report_config, or into a setting of the engine
 * settings parts. Each parameter that names a file is read into a run_file. The
 * projection writes them back as the named parameters ORE expects.
 *
 * The engine settings are typed: a flag is a boolean, read from any spelling ORE's
 * parseBool accepts and written as true or false, and a count is an integer.
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
     * A Setup parameter that is neither a choice nor a file is dropped, and
     * dropped_parameters names it. A value that cannot be read as the type of
     * its setting is refused.
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

    /**
     * @brief The warnings an import reports for the Setup parameters it drops.
     *
     * Each warning names its parameter. A parameter that ORE reads and that
     * changes the numbers, such as fixingCutoff, also says that the run will not
     * read it and that its numbers may differ from the original run.
     */
    static std::vector<std::string> dropped_parameter_warnings(const document& v);
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

    /**
     * @brief The as-of date a run document states, when it states one.
     *
     * The parameter is read by name, so a document that mentions the name in a
     * comment, or that writes its attributes in another order, states the same
     * date as one that does not.
     *
     * @return The ISO 8601 date, or nothing when the document states none.
     * @throws std::invalid_argument if the text is not a run document.
     */
    static std::optional<std::string> as_of_date(std::string_view text);

    /**
     * @brief The same run document, priced at @p date.
     *
     * The document is written back through the projection, so the parameters it
     * states are the ones ORE reads and a parameter the model holds nowhere is
     * not invented.
     *
     * @throws std::invalid_argument if the text is not a run document.
     */
    static std::string with_as_of_date(std::string_view text, std::string_view date);
};

}

#endif
