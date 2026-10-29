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
#ifndef ORES_ORE_SERVICE_MESSAGING_PRICING_ENGINES_IMPORT_RUN_HPP
#define ORES_ORE_SERVICE_MESSAGING_PRICING_ENGINES_IMPORT_RUN_HPP

#include "ores.analytics.api/messaging/configuration_document_protocol.hpp"
#include "ores.ore.api/messaging/pricing_engines_protocol.hpp"
#include "ores.ore.service/export.hpp"
#include "ores.utility/domain/protocol.hpp"
#include <boost/uuid/uuid.hpp>
#include <functional>
#include <set>
#include <string>

namespace ores::ore::service::messaging {

/**
 * @brief What an import asks of the world outside it, so the order it does things
 * in is stated in one place a test can drive.
 *
 * Each member is one operation the handler wires to a message under the caller's
 * delegated session. A caller may throw instead of answering: the run treats a
 * throw as a refusal and undoes what it registered.
 */
struct pricing_engines_import_operations {
    /** The product types the vocabulary holds, read from analytics. */
    std::function<std::set<std::string>()> known_types;

    /**
     * @brief The document the target holds, and the reason when analytics
     * refuses to answer.
     */
    std::function<ores::analytics::messaging::pricing_engines_document(
        const std::string& configuration_id, std::string& error)>
        read_target;

    /** Registers a reporting configuration for a document the import creates. */
    std::function<boost::uuids::uuid(const std::string& name,
                                     const ores::utility::domain::change_intent& intent)>
        register_configuration;

    /** Removes the reporting configuration a failed import registered. */
    std::function<void(const std::string& name, const ores::utility::domain::change_intent& intent)>
        remove_configuration;

    /** Saves the document through the analytics save the screen itself uses. */
    std::function<ores::analytics::messaging::save_pricing_engines_document_response(
        ores::analytics::messaging::pricing_engines_document document)>
        save;
};

/**
 * @brief Writes a pricing engines document into a configuration, or into a new
 * one, and answers what it saved.
 *
 * The run reads everything that can refuse before it registers anything: the
 * file, the target's document and the vocabulary. A configuration registered for
 * a file that is then refused is removed again, so a failed import leaves
 * nothing behind. Both of those are the reason this is a function rather than a
 * stretch of the handler: the reviewer of the import found a header created
 * before a read that could fail, and left behind when it did.
 */
ORES_ORE_SERVICE_EXPORT ::ores::ore::messaging::import_pricing_engines_response
run_pricing_engines_import(const pricing_engines_import_operations& operations,
                           const ::ores::ore::messaging::import_pricing_engines_request& request);

}

#endif
