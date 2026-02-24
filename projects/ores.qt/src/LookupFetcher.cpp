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
#include "ores.qt/LookupFetcher.hpp"
#include "ores.qt/ClientManager.hpp"
#include <boost/uuid/uuid_io.hpp>
#include "ores.refdata/messaging/party_type_protocol.hpp"
#include "ores.refdata/messaging/party_status_protocol.hpp"
#include "ores.refdata/messaging/business_centre_protocol.hpp"
#include "ores.refdata/messaging/currency_protocol.hpp"
#include "ores.refdata/messaging/portfolio_protocol.hpp"
#include "ores.iam/messaging/tenant_type_protocol.hpp"
#include "ores.iam/messaging/tenant_status_protocol.hpp"
#include "ores.comms/messaging/frame.hpp"

namespace ores::qt {

using namespace comms::messaging;

lookup_result fetch_party_lookups(ClientManager* cm) {
    lookup_result result;
    if (!cm) return result;

    {
        refdata::messaging::get_party_types_request request;
        auto payload = request.serialize();
        frame request_frame(message_type::get_party_types_request,
            0, std::move(payload));
        auto response_result = cm->sendRequest(std::move(request_frame));
        if (response_result) {
            auto payload_result = response_result->decompressed_payload();
            if (payload_result) {
                auto response = refdata::messaging::
                    get_party_types_response::deserialize(*payload_result);
                if (response) {
                    for (const auto& t : response->types) {
                        result.type_codes.push_back(t.code);
                    }
                }
            }
        }
    }

    {
        refdata::messaging::get_party_statuses_request request;
        auto payload = request.serialize();
        frame request_frame(message_type::get_party_statuses_request,
            0, std::move(payload));
        auto response_result = cm->sendRequest(std::move(request_frame));
        if (response_result) {
            auto payload_result = response_result->decompressed_payload();
            if (payload_result) {
                auto response = refdata::messaging::
                    get_party_statuses_response::deserialize(*payload_result);
                if (response) {
                    for (const auto& s : response->statuses) {
                        result.status_codes.push_back(s.code);
                    }
                }
            }
        }
    }

    {
        refdata::messaging::get_business_centres_request request;
        request.limit = 1000;
        auto response = cm->process_authenticated_request(std::move(request));
        if (response) {
            for (const auto& bc : response->business_centres) {
                result.business_centre_codes.push_back(bc.code);
            }
        }
    }

    return result;
}

lookup_result fetch_tenant_lookups(ClientManager* cm) {
    lookup_result result;
    if (!cm) return result;

    {
        iam::messaging::get_tenant_types_request request;
        auto payload = request.serialize();
        frame request_frame(message_type::get_tenant_types_request,
            0, std::move(payload));
        auto response_result = cm->sendRequest(std::move(request_frame));
        if (response_result) {
            auto payload_result = response_result->decompressed_payload();
            if (payload_result) {
                auto response = iam::messaging::
                    get_tenant_types_response::deserialize(*payload_result);
                if (response) {
                    for (const auto& t : response->types) {
                        result.type_codes.push_back(t.type);
                    }
                }
            }
        }
    }

    {
        iam::messaging::get_tenant_statuses_request request;
        auto payload = request.serialize();
        frame request_frame(message_type::get_tenant_statuses_request,
            0, std::move(payload));
        auto response_result = cm->sendRequest(std::move(request_frame));
        if (response_result) {
            auto payload_result = response_result->decompressed_payload();
            if (payload_result) {
                auto response = iam::messaging::
                    get_tenant_statuses_response::deserialize(*payload_result);
                if (response) {
                    for (const auto& s : response->statuses) {
                        result.status_codes.push_back(s.status);
                    }
                }
            }
        }
    }

    return result;
}

std::vector<std::string> fetch_currency_codes(ClientManager* cm) {
    std::vector<std::string> codes;
    if (!cm) return codes;

    refdata::messaging::get_currencies_request request;
    request.limit = 1000;
    auto response = cm->process_authenticated_request(std::move(request));
    if (response) {
        for (const auto& ccy : response->currencies) {
            codes.push_back(ccy.iso_code);
        }
    }
    return codes;
}

std::unordered_map<std::string, std::string>
fetch_business_centre_image_map(ClientManager* cm) {
    std::unordered_map<std::string, std::string> mapping;
    if (!cm) return mapping;

    refdata::messaging::get_business_centres_request request;
    request.limit = 1000;
    auto response = cm->process_authenticated_request(std::move(request));
    if (response) {
        for (const auto& bc : response->business_centres) {
            std::string image_id_str;
            if (bc.image_id)
                image_id_str = boost::uuids::to_string(*bc.image_id);
            mapping.emplace(bc.code, std::move(image_id_str));
        }
    }
    return mapping;
}

std::vector<portfolio_entry> fetch_portfolio_entries(ClientManager* cm) {
    std::vector<portfolio_entry> entries;
    if (!cm) return entries;

    refdata::messaging::get_portfolios_request request;
    request.limit = 1000;
    auto response = cm->process_authenticated_request(std::move(request));
    if (response) {
        for (const auto& pf : response->portfolios) {
            entries.push_back({boost::uuids::to_string(pf.id), pf.name});
        }
    }
    return entries;
}

}
