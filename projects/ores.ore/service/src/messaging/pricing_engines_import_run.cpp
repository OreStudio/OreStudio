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
#include "ores.ore.service/messaging/pricing_engines_import_run.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.ore.core/domain/pricing_engines_import.hpp"
#include <boost/uuid/uuid_io.hpp>
#include <format>
#include <map>
#include <stdexcept>
#include <string>
#include <utility>
#include <vector>

namespace ores::ore::service::messaging {

using namespace ores::logging;
using ores::analytics::messaging::pricing_engines_document;
using ores::ore::domain::pricing_engines_import;

namespace {

inline std::string_view run_logger_name = "ores.ore.service.messaging.pricing_engines_import_run";

auto& run_lg() {
    static auto instance = make_logger(run_logger_name);
    return instance;
}

/**
 * @brief The reason the write carries when the person states none.
 *
 * The save keeps the reason a row names, and the import states one on every row,
 * so this is only the floor a shell command may fall to.
 */
constexpr std::string_view default_reason_code = "system.import";
constexpr std::string_view default_commentary = "Imported from an ORE pricing engines document";

}

::ores::ore::messaging::import_pricing_engines_response
run_pricing_engines_import(const pricing_engines_import_operations& operations,
                           const ::ores::ore::messaging::import_pricing_engines_request& request) {
    ::ores::ore::messaging::import_pricing_engines_response response;

    const auto file = pricing_engines_import::parse(request.content);
    if (!file.ok()) {
        BOOST_LOG_SEV(run_lg(), info) << "pricing engines import refused | " << file.refusal;
        response.message = file.refusal;
        return response;
    }

    const ores::utility::domain::change_intent intent{
        .reason_code =
            request.reason_code.empty() ? std::string(default_reason_code) : request.reason_code,
        .commentary =
            request.commentary.empty() ? std::string(default_commentary) : request.commentary};

    // The configuration a failed run registered, so it can be undone however the
    // run fails.
    std::string registered;
    const auto rollback = [&] {
        if (registered.empty())
            return;
        const auto name = registered;
        registered.clear();
        operations.remove_configuration(name, intent);
    };

    try {
        // Everything that can refuse is read before anything is written: the
        // target, the vocabulary and the difference. A configuration registered
        // first would be a header left behind by a file that was refused.
        std::string read_error;
        auto target = operations.read_target(request.target_configuration_id, read_error);
        if (!read_error.empty()) {
            response.message = read_error;
            return response;
        }

        std::string name;
        std::string variant;
        if (!request.target_configuration_id.empty()) {
            name = target.config.name;
            variant = target.config.config_variant;
        } else {
            name = request.new_configuration_name;
            variant = request.new_configuration_variant;
            if (name.empty()) {
                response.message = "A new configuration needs a name.";
                return response;
            }
        }

        const auto difference =
            pricing_engines_import::difference(file, target, operations.known_types());

        // The configuration the document fills: the target's own, or the one the
        // caller is about to register. The rows are stamped with it, so a row the
        // parser minted for a configuration that does not exist cannot be written.
        const auto configuration =
            request.target_configuration_id.empty() ? file.document.config.id : target.config.id;

        auto document =
            pricing_engines_import::merge(file, target, request.removals_kept, configuration);
        document.config.name = name;
        document.config.config_variant = variant;
        // The document save carries no reason of its own, so the reason travels
        // on the rows, which the save keeps when a row names one.
        document.config.change_reason_code = intent.reason_code;
        document.config.change_commentary = intent.commentary;
        for (auto& row : document.products) {
            row.change_reason_code = intent.reason_code;
            row.change_commentary = intent.commentary;
        }
        for (auto& row : document.parameters) {
            row.change_reason_code = intent.reason_code;
            row.change_commentary = intent.commentary;
        }

        const auto keys = pricing_engines_import::product_keys(document);

        // Only now, with everything that can refuse already read, is a
        // configuration registered for the file. The name is remembered before
        // the call, because a registration whose reply is lost has still made a
        // header, and that is what a failed run has to undo.
        if (request.target_configuration_id.empty()) {
            registered = name;
            document.config.configuration_id = operations.register_configuration(name, intent);
        } else {
            // The target was read by the reporting configuration the request
            // names, and the document it answered with carries that same id. A
            // second parse of the request's text could only disagree with it.
            document.config.configuration_id = target.config.configuration_id;
        }

        const auto saved = operations.save(std::move(document));
        if (!saved.success) {
            rollback();
            response.message =
                saved.message.empty() ? "The configuration was refused." : saved.message;
            for (const auto& problem : saved.problems) {
                std::string key;
                if (!problem.product_id.empty()) {
                    for (const auto& [id, product_key] : keys)
                        if (boost::uuids::to_string(id) == problem.product_id)
                            key = problem.parameter_name.empty() ?
                                      product_key :
                                      std::format("{}/{}/{}",
                                                  product_key,
                                                  problem.parameter_scope,
                                                  problem.parameter_name);
                }
                response.errors.push_back({.key = key, .message = problem.message});
            }
            BOOST_LOG_SEV(run_lg(), warn)
                << "pricing engines import refused | " << response.message;
            return response;
        }

        response.success = true;
        response.configuration_id = saved.id;
        response.configuration_name = name;
        response.added = difference.added;
        response.changed = difference.changed;
        for (const auto& row : difference.rows) {
            if (row.change != "removed")
                continue;
            if (std::ranges::find(request.removals_kept, row.key) != request.removals_kept.end())
                ++response.kept;
            else
                ++response.deleted;
        }
        response.message = std::format("Imported: {} added, {} changed, {} deleted, {} kept.",
                                       response.added,
                                       response.changed,
                                       response.deleted,
                                       response.kept);
        return response;
    } catch (const std::exception& e) {
        // A read or a write that threw after the configuration was registered
        // undoes it, so a refused import leaves nothing behind.
        try {
            rollback();
        } catch (const std::exception& rollback_error) {
            BOOST_LOG_SEV(run_lg(), warn) << "pricing engines import could not remove the "
                                             "configuration it registered | "
                                          << registered << " | " << rollback_error.what();
        }
        BOOST_LOG_SEV(run_lg(), warn) << "pricing engines import refused | " << e.what();
        response.message = e.what();
        return response;
    }
}

}
