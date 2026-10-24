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
#ifndef ORES_MARKETDATA_CORE_SERVICE_COMPOSITE_INPUT_SERVICE_HPP
#define ORES_MARKETDATA_CORE_SERVICE_COMPOSITE_INPUT_SERVICE_HPP

#include "ores.database/domain/context.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.marketdata.api/domain/composite_input.hpp"
#include "ores.marketdata.api/messaging/composite_input_protocol.hpp"
#include "ores.marketdata.core/export.hpp"
#include "ores.marketdata.core/repository/composite_input_repository.hpp"
#include <chrono>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::marketdata::service {

/**
 * @brief Service for managing composite inputs.
 *
 * Provides a higher-level interface for composite input operations,
 * wrapping the underlying repository.
 */
class ORES_MARKETDATA_CORE_EXPORT composite_input_service {
private:
    inline static std::string_view logger_name = "ores.marketdata.service.composite_input_service";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    /**
     * @brief Constructs a composite_input_service with a database context.
     *
     * @param ctx The database context for operations.
     */
    explicit composite_input_service(context ctx);

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
    messaging::list_composite_inputs_response
    list_composite_inputs(const messaging::list_composite_inputs_request& request);
    messaging::get_composite_input_response
    get_composite_input(const messaging::get_composite_input_request& request);
    messaging::get_many_composite_inputs_response
    get_many_composite_inputs(const messaging::get_many_composite_inputs_request& request);
    messaging::put_composite_input_response
    put_composite_input(const messaging::put_composite_input_request& request);
    messaging::put_many_composite_inputs_response
    put_many_composite_inputs(const messaging::put_many_composite_inputs_request& request);
    messaging::delete_composite_input_response
    delete_composite_input(const messaging::delete_composite_input_request& request);
    messaging::delete_many_composite_inputs_response
    delete_many_composite_inputs(const messaging::delete_many_composite_inputs_request& request);
    messaging::list_composite_input_versions_response
    list_composite_input_versions(const messaging::list_composite_input_versions_request& request);
    messaging::get_composite_input_version_response
    get_composite_input_version(const messaging::get_composite_input_version_request& request);
    /**@}*/

    /**
     * @brief Lists composite inputs with pagination support.
     *
     * @param offset Number of records to skip.
     * @param limit Maximum number of records to return.
     * @return Vector of composite inputs for the requested page.
     */
    std::vector<domain::composite_input> list_composite_inputs(std::uint32_t offset,
                                                               std::uint32_t limit);

    /**
     * @brief Gets the total count of active composite inputs.
     *
     * @return Total number of active composite inputs.
     */
    std::uint32_t count_composite_inputs();


    /**
     * @brief Retrieves a single composite input as it stood at a specific
     * version. See the "Temporal composite entity versioning" architecture doc.
     *
     * @param version The version to fetch.
     * @return The composite input at that version if found, std::nullopt otherwise.
     */
    std::optional<domain::composite_input>
    get_composite_input_at_version(const boost::uuids::uuid& id, std::uint32_t version);

    /**
     * @brief Retrieves a single composite input by its primary key.
     *
     * The storage key is a uuid, so the signature says which key is meant and
     * the human-readable key cannot be passed here by mistake.
     *
     * @return The composite input if found, std::nullopt otherwise.
     */
    std::optional<domain::composite_input> get_composite_input(const boost::uuids::uuid& id);

    /**
     * @brief Retrieves a single composite input by the key the model
     * declares -- the human-readable key a caller holds.
     *
     * This is the counterpart of the uuid overload above: the two keys an
     * entity holds are different keys, and a call site has to say which one it
     * means.
     *
     * @return The composite input if found, std::nullopt otherwise.
     */
    std::optional<domain::composite_input>
    get_composite_input_by_oresmd_uri(const std::string& oresmd_uri);

    /**
     * @brief Retrieves a batch of composite inputs by primary key.
     */
    std::vector<domain::composite_input> get_composite_inputs(const std::vector<std::string>& ids);

    /**
     * @brief Saves a composite input (creates or updates).
     *
     * @param composite_input The composite input to save.
     * @throws std::exception on failure.
     */
    void save_composite_input(const domain::composite_input& composite_input);

    /**
     * @brief Saves a batch of composite inputs.
     *
     * @param composite_inputs The composite inputs to save.
     * @throws std::exception on failure.
     */
    void save_composite_inputs(const std::vector<domain::composite_input>& composite_inputs);

    /**
     * @brief Deletes a composite input by its primary key.
     *
     * @throws std::exception on failure.
     */
    void delete_composite_input(const boost::uuids::uuid& id);

    /**
     * @brief Deletes composite inputs by their primary keys.
     */
    void delete_composite_inputs(const std::vector<std::string>& ids);

    /**
     * @brief Retrieves all historical versions of a composite input.
     *
     * Addressed by the key the model declares, which is the one a caller
     * holds; the storage key is resolved from it here, the same step every
     * other read makes.
     */
    std::vector<domain::composite_input> get_composite_input_history(const std::string& key);

private:
    context ctx_;
    repository::composite_input_repository repo_;

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
    ores::utility::domain::result prepare_change(const messaging::composite_input_change& change,
                                                 const ores::utility::domain::change_intent& intent,
                                                 domain::composite_input& out);
};

}

#endif
