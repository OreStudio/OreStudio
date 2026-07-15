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
#include "ores.dq.api/messaging/coding_scheme_protocol.hpp"
#include "ores.iam.api/messaging/tenant_status_protocol.hpp"
#include "ores.iam.api/messaging/tenant_type_protocol.hpp"
#include "ores.qt/ClientManager.hpp"
#include "ores.refdata.api/messaging/asset_class_code_protocol.hpp"
#include "ores.refdata.api/messaging/book_status_protocol.hpp"
#include "ores.refdata.api/messaging/business_centre_protocol.hpp"
#include "ores.refdata.api/messaging/business_unit_protocol.hpp"
#include "ores.refdata.api/messaging/calendar_type_protocol.hpp"
#include "ores.refdata.api/messaging/contact_type_protocol.hpp"
#include "ores.refdata.api/messaging/counterparty_protocol.hpp"
#include "ores.refdata.api/messaging/country_protocol.hpp"
#include "ores.refdata.api/messaging/crm_topology_config_protocol.hpp"
#include "ores.refdata.api/messaging/currency_market_tier_protocol.hpp"
#include "ores.refdata.api/messaging/currency_pair_protocol.hpp"
#include "ores.refdata.api/messaging/currency_protocol.hpp"
#include "ores.refdata.api/messaging/instrument_code_protocol.hpp"
#include "ores.refdata.api/messaging/monetary_nature_protocol.hpp"
#include "ores.refdata.api/messaging/party_id_scheme_protocol.hpp"
#include "ores.refdata.api/messaging/party_protocol.hpp"
#include "ores.refdata.api/messaging/party_status_protocol.hpp"
#include "ores.refdata.api/messaging/party_type_protocol.hpp"
#include "ores.refdata.api/messaging/portfolio_protocol.hpp"
#include "ores.refdata.api/messaging/regulatory_book_type_protocol.hpp"
#include "ores.refdata.api/messaging/rounding_type_protocol.hpp"
#include "ores.refdata.api/messaging/tenor_anchor_protocol.hpp"
#include "ores.refdata.api/messaging/tenor_kind_protocol.hpp"
#include "ores.refdata.api/messaging/tenor_resolution_algorithm_protocol.hpp"
#include "ores.refdata.api/messaging/tenor_unit_protocol.hpp"
#include <boost/uuid/uuid_io.hpp>

namespace ores::qt {

lookup_result fetch_party_lookups(ClientManager* cm) {
    lookup_result result;
    if (!cm)
        return result;

    {
        refdata::messaging::get_party_types_request request;
        auto response_result = cm->process_authenticated_request(std::move(request));
        if (response_result) {
            for (const auto& t : response_result->types) {
                result.type_codes.push_back(t.code);
            }
        }
    }

    {
        refdata::messaging::get_party_statuses_request request;
        auto response_result = cm->process_authenticated_request(std::move(request));
        if (response_result) {
            for (const auto& s : response_result->statuses) {
                result.status_codes.push_back(s.code);
            }
        }
    }

    {
        refdata::messaging::get_business_centres_request request;
        request.limit = lookup_fetch_limit;
        auto response = cm->process_authenticated_request(std::move(request));
        if (response) {
            for (const auto& bc : response->centres) {
                result.business_centre_codes.push_back(bc.code);
            }
        }
    }

    return result;
}

lookup_result fetch_tenant_lookups(ClientManager* cm) {
    lookup_result result;
    if (!cm)
        return result;

    {
        iam::messaging::get_tenant_types_request request;
        auto response_result = cm->process_authenticated_request(std::move(request));
        if (response_result) {
            for (const auto& t : response_result->types) {
                result.type_codes.push_back(t.type);
            }
        }
    }

    {
        iam::messaging::get_tenant_statuses_request request;
        auto response_result = cm->process_authenticated_request(std::move(request));
        if (response_result) {
            for (const auto& s : response_result->statuses) {
                result.status_codes.push_back(s.status);
            }
        }
    }

    return result;
}

std::vector<std::string> fetch_currency_codes(ClientManager* cm) {
    std::vector<std::string> codes;
    if (!cm)
        return codes;

    refdata::messaging::get_currencies_request request;
    request.limit = lookup_fetch_limit;
    auto response = cm->process_authenticated_request(std::move(request));
    if (response) {
        for (const auto& ccy : response->currencies) {
            codes.push_back(ccy.iso_code);
        }
    }
    return codes;
}

std::vector<std::string> fetch_currency_pair_codes(ClientManager* cm) {
    std::vector<std::string> codes;
    if (!cm)
        return codes;

    refdata::messaging::get_currency_pairs_request request;
    request.limit = lookup_fetch_limit;
    auto response = cm->process_authenticated_request(std::move(request));
    if (response) {
        for (const auto& pair : response->pairs) {
            codes.push_back(pair.pair_code);
        }
    }
    return codes;
}

std::unordered_map<std::string, std::string> fetch_currency_names(ClientManager* cm) {
    std::unordered_map<std::string, std::string> names;
    if (!cm)
        return names;

    refdata::messaging::get_currencies_request request;
    request.limit = lookup_fetch_limit;
    auto response = cm->process_authenticated_request(std::move(request));
    if (response) {
        for (const auto& ccy : response->currencies) {
            names.emplace(ccy.iso_code, ccy.name);
        }
    }
    return names;
}

std::vector<std::string> fetch_country_codes(ClientManager* cm) {
    std::vector<std::string> codes;
    if (!cm)
        return codes;

    refdata::messaging::get_countries_request request;
    request.limit = lookup_fetch_limit;
    auto response = cm->process_authenticated_request(std::move(request));
    if (response) {
        for (const auto& country : response->countries) {
            codes.push_back(country.alpha2_code);
        }
    }
    return codes;
}

std::vector<std::string> fetch_contact_type_codes(ClientManager* cm) {
    std::vector<std::string> codes;
    if (!cm)
        return codes;

    refdata::messaging::get_contact_types_request request;
    request.limit = lookup_fetch_limit;
    auto response = cm->process_authenticated_request(std::move(request));
    if (response) {
        for (const auto& type : response->types) {
            codes.push_back(type.code);
        }
    }
    return codes;
}

std::vector<std::string> fetch_party_id_scheme_codes(ClientManager* cm) {
    std::vector<std::string> codes;
    if (!cm)
        return codes;

    refdata::messaging::get_party_id_schemes_request request;
    request.limit = lookup_fetch_limit;
    auto response = cm->process_authenticated_request(std::move(request));
    if (response) {
        for (const auto& scheme : response->party_id_schemes) {
            codes.push_back(scheme.code);
        }
    }
    return codes;
}

std::unordered_map<std::string, std::string> fetch_business_centre_country_map(ClientManager* cm) {
    std::unordered_map<std::string, std::string> mapping;
    if (!cm)
        return mapping;

    refdata::messaging::get_business_centres_request request;
    request.limit = lookup_fetch_limit;
    auto response = cm->process_authenticated_request(std::move(request));
    if (response) {
        for (const auto& bc : response->centres) {
            mapping.emplace(bc.code, bc.country_alpha2_code);
        }
    }
    return mapping;
}

std::vector<std::string> fetch_business_centre_codes(ClientManager* cm) {
    std::vector<std::string> codes;
    if (!cm)
        return codes;

    refdata::messaging::get_business_centres_request request;
    request.limit = lookup_fetch_limit;
    auto response = cm->process_authenticated_request(std::move(request));
    if (response) {
        for (const auto& bc : response->centres) {
            codes.push_back(bc.code);
        }
    }
    return codes;
}

std::vector<portfolio_entry> fetch_portfolio_entries(ClientManager* cm) {
    std::vector<portfolio_entry> entries;
    if (!cm)
        return entries;

    refdata::messaging::get_portfolios_request request;
    request.limit = lookup_fetch_limit;
    auto response = cm->process_authenticated_request(std::move(request));
    if (response) {
        for (const auto& pf : response->portfolios) {
            entries.push_back({boost::uuids::to_string(pf.id), pf.name});
        }
    }
    return entries;
}

std::vector<business_unit_entry> fetch_business_unit_entries(ClientManager* cm) {
    std::vector<business_unit_entry> entries;
    if (!cm)
        return entries;

    refdata::messaging::get_business_units_request request;
    request.limit = lookup_fetch_limit;
    auto response = cm->process_authenticated_request(std::move(request));
    if (response) {
        for (const auto& bu : response->business_units) {
            entries.push_back({boost::uuids::to_string(bu.id), bu.unit_name});
        }
    }
    return entries;
}

std::expected<std::vector<refdata::domain::book_status>, QString>
fetch_book_statuses(ClientManager* cm) {
    if (!cm)
        return std::unexpected(QStringLiteral("Not connected to server."));

    refdata::messaging::get_book_statuses_request request;
    auto response = cm->process_authenticated_request(std::move(request));
    if (!response)
        return std::unexpected(QString::fromStdString(response.error()));
    return std::move(response->statuses);
}

std::expected<std::vector<refdata::domain::party_type>, QString>
fetch_party_types(ClientManager* cm) {
    if (!cm)
        return std::unexpected(QStringLiteral("Not connected to server."));

    refdata::messaging::get_party_types_request request;
    auto response = cm->process_authenticated_request(std::move(request));
    if (!response)
        return std::unexpected(QString::fromStdString(response.error()));
    return std::move(response->types);
}

std::expected<std::vector<refdata::domain::party_status>, QString>
fetch_party_statuses(ClientManager* cm) {
    if (!cm)
        return std::unexpected(QStringLiteral("Not connected to server."));

    refdata::messaging::get_party_statuses_request request;
    auto response = cm->process_authenticated_request(std::move(request));
    if (!response)
        return std::unexpected(QString::fromStdString(response.error()));
    return std::move(response->statuses);
}

std::expected<std::vector<refdata::domain::party_id_scheme>, QString>
fetch_party_id_schemes(ClientManager* cm) {
    if (!cm)
        return std::unexpected(QStringLiteral("Not connected to server."));

    refdata::messaging::get_party_id_schemes_request request;
    auto response = cm->process_authenticated_request(std::move(request));
    if (!response)
        return std::unexpected(QString::fromStdString(response.error()));
    return std::move(response->party_id_schemes);
}

std::expected<std::vector<refdata::domain::contact_type>, QString>
fetch_contact_types(ClientManager* cm) {
    if (!cm)
        return std::unexpected(QStringLiteral("Not connected to server."));

    refdata::messaging::get_contact_types_request request;
    auto response = cm->process_authenticated_request(std::move(request));
    if (!response)
        return std::unexpected(QString::fromStdString(response.error()));
    return std::move(response->types);
}

std::expected<std::vector<refdata::domain::party>, QString>
fetch_parties(ClientManager* cm) {
    if (!cm)
        return std::unexpected(QStringLiteral("Not connected to server."));

    refdata::messaging::get_parties_request request;
    request.limit = 1000;
    auto response = cm->process_authenticated_request(std::move(request));
    if (!response)
        return std::unexpected(QString::fromStdString(response.error()));
    return std::move(response->parties);
}

std::expected<std::vector<refdata::domain::counterparty>, QString>
fetch_counterparties(ClientManager* cm) {
    if (!cm)
        return std::unexpected(QStringLiteral("Not connected to server."));

    refdata::messaging::get_counterparties_request request;
    request.limit = 1000;
    auto response = cm->process_authenticated_request(std::move(request));
    if (!response)
        return std::unexpected(QString::fromStdString(response.error()));
    return std::move(response->counterparties);
}

std::expected<std::vector<refdata::domain::crm_topology_config>, QString>
fetch_crm_topology_configs(ClientManager* cm) {
    if (!cm)
        return std::unexpected(QStringLiteral("Not connected to server."));

    refdata::messaging::get_crm_topology_configs_request request;
    auto response = cm->process_authenticated_request(std::move(request));
    if (!response)
        return std::unexpected(QString::fromStdString(response.error()));
    return std::move(response->crm_topology_configs);
}

std::expected<std::vector<refdata::domain::regulatory_book_type>, QString>
fetch_regulatory_book_types(ClientManager* cm) {
    if (!cm)
        return std::unexpected(QStringLiteral("Not connected to server."));

    refdata::messaging::get_regulatory_book_types_request request;
    auto response = cm->process_authenticated_request(std::move(request));
    if (!response)
        return std::unexpected(QString::fromStdString(response.error()));
    return std::move(response->types);
}

std::expected<std::vector<refdata::domain::tenor_kind>, QString>
fetch_tenor_kinds(ClientManager* cm) {
    if (!cm)
        return std::unexpected(QStringLiteral("Not connected to server."));

    refdata::messaging::get_tenor_kinds_request request;
    auto response = cm->process_authenticated_request(std::move(request));
    if (!response)
        return std::unexpected(QString::fromStdString(response.error()));
    return std::move(response->kinds);
}

std::expected<std::vector<refdata::domain::tenor_unit>, QString>
fetch_tenor_units(ClientManager* cm) {
    if (!cm)
        return std::unexpected(QStringLiteral("Not connected to server."));

    refdata::messaging::get_tenor_units_request request;
    auto response = cm->process_authenticated_request(std::move(request));
    if (!response)
        return std::unexpected(QString::fromStdString(response.error()));
    return std::move(response->units);
}

std::expected<std::vector<refdata::domain::tenor_resolution_algorithm>, QString>
fetch_tenor_resolution_algorithms(ClientManager* cm) {
    if (!cm)
        return std::unexpected(QStringLiteral("Not connected to server."));

    refdata::messaging::get_tenor_resolution_algorithms_request request;
    auto response = cm->process_authenticated_request(std::move(request));
    if (!response)
        return std::unexpected(QString::fromStdString(response.error()));
    return std::move(response->algorithms);
}

std::expected<std::vector<refdata::domain::tenor_anchor>, QString>
fetch_tenor_anchors(ClientManager* cm) {
    if (!cm)
        return std::unexpected(QStringLiteral("Not connected to server."));

    refdata::messaging::get_tenor_anchors_request request;
    auto response = cm->process_authenticated_request(std::move(request));
    if (!response)
        return std::unexpected(QString::fromStdString(response.error()));
    return std::move(response->anchors);
}

std::expected<std::vector<dq::domain::coding_scheme>, QString>
fetch_coding_schemes(ClientManager* cm) {
    if (!cm)
        return std::unexpected(QStringLiteral("Not connected to server."));

    dq::messaging::get_coding_schemes_request request;
    request.limit = lookup_fetch_limit;
    auto response = cm->process_authenticated_request(std::move(request));
    if (!response)
        return std::unexpected(QString::fromStdString(response.error()));
    return std::move(response->coding_schemes);
}

std::expected<std::vector<refdata::domain::asset_class_code>, QString>
fetch_asset_class_codes(ClientManager* cm) {
    if (!cm)
        return std::unexpected(QStringLiteral("Not connected to server."));

    refdata::messaging::get_asset_class_codes_request request;
    auto response = cm->process_authenticated_request(std::move(request));
    if (!response)
        return std::unexpected(QString::fromStdString(response.error()));
    return std::move(response->asset_classes);
}

std::expected<std::vector<refdata::domain::instrument_code>, QString>
fetch_instrument_codes(ClientManager* cm) {
    if (!cm)
        return std::unexpected(QStringLiteral("Not connected to server."));

    refdata::messaging::get_instrument_codes_request request;
    auto response = cm->process_authenticated_request(std::move(request));
    if (!response)
        return std::unexpected(QString::fromStdString(response.error()));
    return std::move(response->instruments);
}

std::expected<std::vector<refdata::domain::rounding_type>, QString>
fetch_rounding_types(ClientManager* cm) {
    if (!cm)
        return std::unexpected(QStringLiteral("Not connected to server."));

    refdata::messaging::get_rounding_types_request request;
    auto response = cm->process_authenticated_request(std::move(request));
    if (!response)
        return std::unexpected(QString::fromStdString(response.error()));
    return std::move(response->types);
}

std::expected<std::vector<refdata::domain::monetary_nature>, QString>
fetch_monetary_natures(ClientManager* cm) {
    if (!cm)
        return std::unexpected(QStringLiteral("Not connected to server."));

    refdata::messaging::get_monetary_natures_request request;
    auto response = cm->process_authenticated_request(std::move(request));
    if (!response)
        return std::unexpected(QString::fromStdString(response.error()));
    return std::move(response->types);
}

std::expected<std::vector<refdata::domain::currency_market_tier>, QString>
fetch_currency_market_tiers(ClientManager* cm) {
    if (!cm)
        return std::unexpected(QStringLiteral("Not connected to server."));

    refdata::messaging::get_currency_market_tiers_request request;
    auto response = cm->process_authenticated_request(std::move(request));
    if (!response)
        return std::unexpected(QString::fromStdString(response.error()));
    return std::move(response->types);
}

std::expected<std::vector<refdata::domain::calendar_type>, QString>
fetch_calendar_types(ClientManager* cm) {
    if (!cm)
        return std::unexpected(QStringLiteral("Not connected to server."));

    refdata::messaging::get_calendar_types_request request;
    auto response = cm->process_authenticated_request(std::move(request));
    if (!response)
        return std::unexpected(QString::fromStdString(response.error()));
    return std::move(response->types);
}

std::expected<std::vector<refdata::domain::country>, QString> fetch_countries(ClientManager* cm) {
    if (!cm)
        return std::unexpected(QStringLiteral("Not connected to server."));

    refdata::messaging::get_countries_request request;
    auto response = cm->process_authenticated_request(std::move(request));
    if (!response)
        return std::unexpected(QString::fromStdString(response.error()));
    return std::move(response->countries);
}

}
