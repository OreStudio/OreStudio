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
#ifndef ORES_DQ_API_MESSAGING_PUBLISH_BUNDLE_PROTOCOL_HPP
#define ORES_DQ_API_MESSAGING_PUBLISH_BUNDLE_PROTOCOL_HPP

#include <string>
#include <vector>
#include <optional>
#include <rfl/json.hpp>
#include "ores.dq.api/domain/publication_mode.hpp"

namespace ores::dq::messaging {

struct lei_parties_params {
    std::string root_lei;
};

struct publish_bundle_params {
    std::vector<std::string> opted_in_datasets;
    std::optional<lei_parties_params> lei_parties;
};

inline std::string build_params_json(const publish_bundle_params& params) {
    return rfl::json::write(params);
}

struct bundle_dataset_result {
    std::string dataset_code;
    bool success = false;
    std::string error_message;
    int records_inserted = 0;
    int records_updated = 0;
    int records_skipped = 0;
    int records_deleted = 0;
};

struct publish_bundle_request {
    using response_type = struct publish_bundle_response;
    static constexpr std::string_view nats_subject = "dq.v1.bundles.publish";
    std::string bundle_code;
    ores::dq::domain::publication_mode mode =
        ores::dq::domain::publication_mode::upsert;
    std::string published_by;
    bool atomic = false;
    std::string params_json;
};

struct publish_bundle_response {
    bool success = false;
    std::string error_message;
    int datasets_succeeded = 0;
    int total_records_inserted = 0;
    int total_records_updated = 0;
    std::vector<bundle_dataset_result> dataset_results;
};

}

#endif
