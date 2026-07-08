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
#ifndef ORES_REFDATA_CORE_REPOSITORY_PARTY_CONTACT_INFORMATION_REPOSITORY_HPP
#define ORES_REFDATA_CORE_REPOSITORY_PARTY_CONTACT_INFORMATION_REPOSITORY_HPP

#include "ores.database/domain/context.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.refdata.api/domain/party_contact_information.hpp"
#include "ores.refdata.core/export.hpp"
#include <boost/uuid/uuid.hpp>
#include <chrono>
#include <optional>
#include <sqlgen/postgres.hpp>
#include <string>
#include <vector>

namespace ores::refdata::repository {

/**
 * @brief Reads and writes party contact informations to data storage.
 */
class ORES_REFDATA_CORE_EXPORT party_contact_information_repository {
private:
    inline static std::string_view logger_name =
        "ores.refdata.repository.party_contact_information_repository";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    explicit party_contact_information_repository(context ctx);

    std::string sql();

    void write(const domain::party_contact_information& party_contact_information);
    void write(const std::vector<domain::party_contact_information>& party_contact_informations);

    std::vector<domain::party_contact_information> read_latest();
    std::vector<domain::party_contact_information> read_latest(const boost::uuids::uuid& id);
    std::vector<domain::party_contact_information> read_latest_by_code(const std::string& code);
    std::vector<domain::party_contact_information>
    read_latest_by_party_id(const boost::uuids::uuid& party_id);

    std::vector<domain::party_contact_information> read_all(const boost::uuids::uuid& id);
    void remove(const boost::uuids::uuid& id);

    /**
     * @brief Reads a single party contact information as it stood at a
     * specific version. See the "Temporal composite entity versioning"
     * architecture doc.
     */
    std::optional<domain::party_contact_information>
    read_at_version(const boost::uuids::uuid& id, std::uint32_t version);

    /**
     * @brief Reads party contact informations filtered by party_id that were
     * live at any point during [valid_from_bound, valid_to_bound) — the set
     * that composes a parent party's state as of one of its own historical
     * versions.
     */
    std::vector<domain::party_contact_information>
    read_by_party_id_as_of(const boost::uuids::uuid& party_id,
                          std::chrono::system_clock::time_point valid_from_bound,
                          std::chrono::system_clock::time_point valid_to_bound);

private:
    context ctx_;
};

}

#endif
