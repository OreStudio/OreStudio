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
/**
 * AUTO-GENERATED FILE - DO NOT EDIT MANUALLY
 * Template: cpp_service.hpp.mustache
 * To modify, update the template and regenerate.
 */
#ifndef ORES_TRADING_CORE_SERVICE_INSTRUMENT_OPTION_SERVICE_HPP
#define ORES_TRADING_CORE_SERVICE_INSTRUMENT_OPTION_SERVICE_HPP

#include "ores.database/domain/context.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.trading.api/domain/instrument_option.hpp"
#include "ores.trading.api/messaging/instrument_option_protocol.hpp"
#include "ores.trading.core/export.hpp"
#include "ores.trading.core/repository/instrument_option_repository.hpp"
#include <chrono>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::trading::service {

/**
 * @brief Service for managing instrument options.
 *
 * Provides a higher-level interface for instrument option operations,
 * wrapping the underlying repository.
 */
class ORES_TRADING_CORE_EXPORT instrument_option_service {
private:
    inline static std::string_view logger_name = "ores.trading.service.instrument_option_service";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    /**
     * @brief Constructs a instrument_option_service with a database context.
     *
     * @param ctx The database context for operations.
     */
    explicit instrument_option_service(context ctx);

    /**
     * @brief The protocol operations, one method per subject.
     *
     * A method takes the canonical request and answers its response, so the
     * handler that serves the subject decodes, calls and replies without
     * deciding anything. The result a caller reads -- missing, conflicting,
     * denied -- is filled here, where the storage call that decided it is
     * made, rather than being inferred from an exception.
     */
    /**@{*/
    messaging::list_instrument_options_response
    list_instrument_options(const messaging::list_instrument_options_request& request);
    messaging::get_instrument_option_response
    get_instrument_option(const messaging::get_instrument_option_request& request);
    messaging::get_many_instrument_options_response
    get_many_instrument_options(const messaging::get_many_instrument_options_request& request);
    messaging::put_instrument_option_response
    put_instrument_option(const messaging::put_instrument_option_request& request);
    messaging::put_many_instrument_options_response
    put_many_instrument_options(const messaging::put_many_instrument_options_request& request);
    messaging::delete_instrument_option_response
    delete_instrument_option(const messaging::delete_instrument_option_request& request);
    messaging::delete_many_instrument_options_response delete_many_instrument_options(
        const messaging::delete_many_instrument_options_request& request);
    messaging::list_instrument_option_versions_response list_instrument_option_versions(
        const messaging::list_instrument_option_versions_request& request);
    messaging::get_instrument_option_version_response
    get_instrument_option_version(const messaging::get_instrument_option_version_request& request);
    /**@}*/

    /**
     * @brief Lists instrument options with pagination support.
     *
     * @param offset Number of records to skip.
     * @param limit Maximum number of records to return.
     * @return Vector of instrument options for the requested page.
     */
    std::vector<domain::instrument_option> list_instrument_options(std::uint32_t offset,
                                                                   std::uint32_t limit);

    /**
     * @brief Gets the total count of active instrument options.
     *
     * @return Total number of active instrument options.
     */
    std::uint32_t count_instrument_options();


    /**
     * @brief Retrieves a single instrument option as it stood at a specific
     * version. See the "Temporal composite entity versioning" architecture doc.
     *
     * @param version The version to fetch.
     * @return The instrument option at that version if found, std::nullopt otherwise.
     */
    std::optional<domain::instrument_option>
    get_instrument_option_at_version(const boost::uuids::uuid& instrument_id,
                                     std::uint32_t version);

    /**
     * @brief Retrieves a single instrument option by its primary key.
     *
     * The storage key is a uuid, so the signature says which key is meant and
     * the human-readable key cannot be passed here by mistake.
     *
     * @return The instrument option if found, std::nullopt otherwise.
     */
    std::optional<domain::instrument_option>
    get_instrument_option(const boost::uuids::uuid& instrument_id);

    /**
     * @brief Retrieves a batch of instrument options by primary key.
     */
    std::vector<domain::instrument_option>
    get_instrument_options(const std::vector<std::string>& instrument_ids);

    /**
     * @brief Saves a instrument option (creates or updates).
     *
     * @param instrument_option The instrument option to save.
     * @throws std::exception on failure.
     */
    void save_instrument_option(const domain::instrument_option& instrument_option);

    /**
     * @brief Saves a batch of instrument options.
     *
     * @param instrument_options The instrument options to save.
     * @throws std::exception on failure.
     */
    void save_instrument_options(const std::vector<domain::instrument_option>& instrument_options);

    /**
     * @brief Deletes a instrument option by its primary key.
     *
     * @throws std::exception on failure.
     */
    void delete_instrument_option(const boost::uuids::uuid& instrument_id);

    /**
     * @brief Deletes instrument options by their primary keys.
     */
    void delete_instrument_options(const std::vector<std::string>& instrument_ids);

    /**
     * @brief Retrieves all historical versions of a instrument option.
     *
     * Addressed by the entity's key, which is its storage key.
     */
    std::vector<domain::instrument_option>
    get_instrument_option_history(const std::string& instrument_id);

private:
    context ctx_;
    repository::instrument_option_repository repo_;

    /**
     * @brief Checks one change against the row it names, and stamps it.
     *
     * A single write and a batch state the same claim, so the check, the
     * server-derived provenance and the version the store must match are one
     * decision made in one place. A batch that made the decision per element
     * would eventually make it differently from the single write.
     *
     * @param change The change as the caller stated it.
     * @param intent The reason and commentary the caller gave.
     * @param out The stamped domain object, written only when the result is ok.
     * @return ok, or why the change was refused.
     */
    ores::utility::domain::result prepare_change(const messaging::instrument_option_change& change,
                                                 const ores::utility::domain::change_intent& intent,
                                                 domain::instrument_option& out);
};

}

#endif
