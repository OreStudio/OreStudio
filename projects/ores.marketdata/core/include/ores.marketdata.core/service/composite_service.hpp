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
#ifndef ORES_MARKETDATA_CORE_SERVICE_COMPOSITE_SERVICE_HPP
#define ORES_MARKETDATA_CORE_SERVICE_COMPOSITE_SERVICE_HPP

#include "ores.database/domain/context.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.marketdata.api/domain/composite.hpp"
#include "ores.marketdata.api/messaging/composite_protocol.hpp"
#include "ores.marketdata.core/export.hpp"
#include "ores.marketdata.core/repository/composite_repository.hpp"
#include <chrono>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::marketdata::service {

/**
 * @brief Service for managing composites.
 *
 * Provides a higher-level interface for composite operations,
 * wrapping the underlying repository.
 */
class ORES_MARKETDATA_CORE_EXPORT composite_service {
private:
    inline static std::string_view logger_name = "ores.marketdata.service.composite_service";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    /**
     * @brief Constructs a composite_service with a database context.
     *
     * @param ctx The database context for operations.
     */
    explicit composite_service(context ctx);

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
    messaging::list_composites_response
    list_composites(const messaging::list_composites_request& request);
    messaging::get_composite_response
    get_composite(const messaging::get_composite_request& request);
    messaging::get_many_composites_response
    get_many_composites(const messaging::get_many_composites_request& request);
    messaging::put_composite_response
    put_composite(const messaging::put_composite_request& request);
    messaging::put_many_composites_response
    put_many_composites(const messaging::put_many_composites_request& request);
    messaging::delete_composite_response
    delete_composite(const messaging::delete_composite_request& request);
    messaging::delete_many_composites_response
    delete_many_composites(const messaging::delete_many_composites_request& request);
    messaging::list_composite_versions_response
    list_composite_versions(const messaging::list_composite_versions_request& request);
    messaging::get_composite_version_response
    get_composite_version(const messaging::get_composite_version_request& request);
    /**@}*/

    /**
     * @brief Lists composites with pagination support.
     *
     * @param offset Number of records to skip.
     * @param limit Maximum number of records to return.
     * @return Vector of composites for the requested page.
     */
    std::vector<domain::composite> list_composites(std::uint32_t offset, std::uint32_t limit);

    /**
     * @brief Gets the total count of active composites.
     *
     * @return Total number of active composites.
     */
    std::uint32_t count_composites();


    /**
     * @brief Retrieves a single composite as it stood at a specific
     * version. See the "Temporal composite entity versioning" architecture doc.
     *
     * @param version The version to fetch.
     * @return The composite at that version if found, std::nullopt otherwise.
     */
    std::optional<domain::composite> get_composite_at_version(const boost::uuids::uuid& id,
                                                              std::uint32_t version);

    /**
     * @brief Retrieves a single composite by its primary key.
     *
     * The storage key is a uuid, so the signature says which key is meant and
     * the human-readable key cannot be passed here by mistake.
     *
     * @return The composite if found, std::nullopt otherwise.
     */
    std::optional<domain::composite> get_composite(const boost::uuids::uuid& id);

    /**
     * @brief Retrieves a single composite by the key the model
     * declares -- the human-readable key a caller holds.
     *
     * This is the counterpart of the uuid overload above: the two keys an
     * entity holds are different keys, and a call site has to say which one it
     * means.
     *
     * @return The composite if found, std::nullopt otherwise.
     */
    std::optional<domain::composite> get_composite_by_name(const std::string& name);

    /**
     * @brief Retrieves a batch of composites by primary key.
     */
    std::vector<domain::composite> get_composites(const std::vector<std::string>& ids);

    /**
     * @brief Saves a composite (creates or updates).
     *
     * @param composite The composite to save.
     * @throws std::exception on failure.
     */
    void save_composite(const domain::composite& composite);

    /**
     * @brief Saves a batch of composites.
     *
     * @param composites The composites to save.
     * @throws std::exception on failure.
     */
    void save_composites(const std::vector<domain::composite>& composites);

    /**
     * @brief Deletes a composite by its primary key.
     *
     * @throws std::exception on failure.
     */
    void delete_composite(const boost::uuids::uuid& id);

    /**
     * @brief Deletes composites by their primary keys.
     */
    void delete_composites(const std::vector<std::string>& ids);

    /**
     * @brief Retrieves all historical versions of a composite.
     *
     * Addressed by the key the model declares, which is the one a caller
     * holds; the storage key is resolved from it here, the same step every
     * other read makes.
     */
    std::vector<domain::composite> get_composite_history(const std::string& key);

private:
    context ctx_;
    repository::composite_repository repo_;

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
    ores::utility::domain::result prepare_change(const messaging::composite_change& change,
                                                 const ores::utility::domain::change_intent& intent,
                                                 domain::composite& out);
};

}

#endif
