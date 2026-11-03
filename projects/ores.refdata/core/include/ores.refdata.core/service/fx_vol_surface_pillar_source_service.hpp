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
#ifndef ORES_REFDATA_CORE_SERVICE_FX_VOL_SURFACE_PILLAR_SOURCE_SERVICE_HPP
#define ORES_REFDATA_CORE_SERVICE_FX_VOL_SURFACE_PILLAR_SOURCE_SERVICE_HPP

#include "ores.database/domain/context.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.refdata.api/domain/fx_vol_surface_pillar_source.hpp"
#include "ores.refdata.api/messaging/fx_vol_surface_pillar_source_protocol.hpp"
#include "ores.refdata.core/export.hpp"
#include "ores.refdata.core/repository/fx_vol_surface_pillar_source_repository.hpp"
#include <chrono>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::refdata::service {

/**
 * @brief Service for managing FX volatility surface pillar sources.
 *
 * Provides a higher-level interface for FX volatility surface pillar source operations,
 * wrapping the underlying repository.
 */
class ORES_REFDATA_CORE_EXPORT fx_vol_surface_pillar_source_service {
private:
    inline static std::string_view logger_name =
        "ores.refdata.service.fx_vol_surface_pillar_source_service";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    /**
     * @brief Constructs a fx_vol_surface_pillar_source_service with a database context.
     *
     * @param ctx The database context for operations.
     */
    explicit fx_vol_surface_pillar_source_service(context ctx);

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
    ::ores::refdata::messaging::list_fx_vol_surface_pillar_sources_response
    list_fx_vol_surface_pillar_sources(
        const ::ores::refdata::messaging::list_fx_vol_surface_pillar_sources_request& request);
    ::ores::refdata::messaging::get_fx_vol_surface_pillar_source_response
    get_fx_vol_surface_pillar_source(
        const ::ores::refdata::messaging::get_fx_vol_surface_pillar_source_request& request);
    ::ores::refdata::messaging::get_many_fx_vol_surface_pillar_sources_response
    get_many_fx_vol_surface_pillar_sources(
        const ::ores::refdata::messaging::get_many_fx_vol_surface_pillar_sources_request& request);
    ::ores::refdata::messaging::put_fx_vol_surface_pillar_source_response
    put_fx_vol_surface_pillar_source(
        const ::ores::refdata::messaging::put_fx_vol_surface_pillar_source_request& request);
    ::ores::refdata::messaging::put_many_fx_vol_surface_pillar_sources_response
    put_many_fx_vol_surface_pillar_sources(
        const ::ores::refdata::messaging::put_many_fx_vol_surface_pillar_sources_request& request);
    ::ores::refdata::messaging::delete_fx_vol_surface_pillar_source_response
    delete_fx_vol_surface_pillar_source(
        const ::ores::refdata::messaging::delete_fx_vol_surface_pillar_source_request& request);
    ::ores::refdata::messaging::delete_many_fx_vol_surface_pillar_sources_response
    delete_many_fx_vol_surface_pillar_sources(
        const ::ores::refdata::messaging::delete_many_fx_vol_surface_pillar_sources_request&
            request);
    ::ores::refdata::messaging::list_fx_vol_surface_pillar_source_versions_response
    list_fx_vol_surface_pillar_source_versions(
        const ::ores::refdata::messaging::list_fx_vol_surface_pillar_source_versions_request&
            request);
    ::ores::refdata::messaging::get_fx_vol_surface_pillar_source_version_response
    get_fx_vol_surface_pillar_source_version(
        const ::ores::refdata::messaging::get_fx_vol_surface_pillar_source_version_request&
            request);
    /**@}*/

    /**
     * @brief Lists FX volatility surface pillar sources with pagination support.
     *
     * @param offset Number of records to skip.
     * @param limit Maximum number of records to return.
     * @return Vector of FX volatility surface pillar sources for the requested page.
     */
    std::vector<domain::fx_vol_surface_pillar_source> list_pillar_sources(std::uint32_t offset,
                                                                          std::uint32_t limit);

    /**
     * @brief Gets the total count of active FX volatility surface pillar sources.
     *
     * @return Total number of active FX volatility surface pillar sources.
     */
    std::uint32_t count_pillar_sources();


    /**
     * @brief Retrieves a single FX volatility surface pillar source as it stood at a specific
     * version. See the "Temporal composite entity versioning" architecture doc.
     *
     * @param version The version to fetch.
     * @return The FX volatility surface pillar source at that version if found, std::nullopt
     * otherwise.
     */
    std::optional<domain::fx_vol_surface_pillar_source>
    get_pillar_source_at_version(const boost::uuids::uuid& id, std::uint32_t version);

    /**
     * @brief Retrieves a single FX volatility surface pillar source by its primary key.
     *
     * The storage key is a uuid, so the signature says which key is meant and
     * the human-readable key cannot be passed here by mistake.
     *
     * @return The FX volatility surface pillar source if found, std::nullopt otherwise.
     */
    std::optional<domain::fx_vol_surface_pillar_source>
    get_pillar_source(const boost::uuids::uuid& id);

    /**
     * @brief Retrieves a batch of FX volatility surface pillar sources by primary key.
     */
    std::vector<domain::fx_vol_surface_pillar_source>
    get_pillar_sources(const std::vector<std::string>& ids);

    /**
     * @brief Saves a FX volatility surface pillar source (creates or updates).
     *
     * @param pillar_source The FX volatility surface pillar source to save.
     * @throws std::exception on failure.
     */
    void save_pillar_source(const domain::fx_vol_surface_pillar_source& pillar_source);

    /**
     * @brief Saves a batch of FX volatility surface pillar sources.
     *
     * @param pillar_sources The FX volatility surface pillar sources to save.
     * @throws std::exception on failure.
     */
    void
    save_pillar_sources(const std::vector<domain::fx_vol_surface_pillar_source>& pillar_sources);

    /**
     * @brief Deletes a FX volatility surface pillar source by its primary key.
     *
     * @throws std::exception on failure.
     */
    void delete_pillar_source(const boost::uuids::uuid& id);

    /**
     * @brief Deletes FX volatility surface pillar sources by their primary keys.
     */
    void delete_pillar_sources(const std::vector<std::string>& ids);

    /**
     * @brief Retrieves all historical versions of a FX volatility surface pillar source.
     *
     * Addressed by the entity's key, which is its storage key.
     */
    std::vector<domain::fx_vol_surface_pillar_source>
    get_pillar_source_history(const std::string& id);

private:
    context ctx_;
    repository::fx_vol_surface_pillar_source_repository repo_;

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
    ores::utility::domain::result
    prepare_change(const ::ores::refdata::messaging::fx_vol_surface_pillar_source_change& change,
                   const ores::utility::domain::change_intent& intent,
                   domain::fx_vol_surface_pillar_source& out);
};

}

#endif
