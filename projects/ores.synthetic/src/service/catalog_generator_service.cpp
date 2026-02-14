/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*-
 *
 * Copyright (C) 2025 Marco Craveiro <marco.craveiro@gmail.com>
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
#include "ores.synthetic/service/catalog_generator_service.hpp"
#include "ores.utility/generation/generation_context.hpp"

#include <array>
#include <sstream>

namespace ores::synthetic::service {

using ores::utility::generation::generation_context;

namespace {

// Predefined realistic data for high-quality generation

const std::array<std::pair<std::string, std::string>, 10> account_data = {{
    {"jsmith", "john.smith@example.com"},
    {"mjohnson", "mary.johnson@example.com"},
    {"rwilliams", "robert.williams@example.com"},
    {"sbrown", "sarah.brown@example.com"},
    {"dlee", "david.lee@example.com"},
    {"ewilson", "emma.wilson@example.com"},
    {"mgarcia", "michael.garcia@example.com"},
    {"jmartinez", "jennifer.martinez@example.com"},
    {"canderson", "christopher.anderson@example.com"},
    {"lthomas", "lisa.thomas@example.com"}
}};

const std::array<std::pair<std::string, std::string>, 8> catalog_data = {{
    {"ISO Standards", "International Organization for Standardization reference data"},
    {"Market Data", "Real-time and historical market pricing data"},
    {"Reference Data", "Static reference data for financial instruments"},
    {"Regulatory", "Data required for regulatory compliance and reporting"},
    {"Risk Analytics", "Risk calculation and analytics datasets"},
    {"Trade Data", "Transaction and trade execution data"},
    {"Client Data", "Client and counterparty information"},
    {"Corporate Actions", "Corporate action events and announcements"}
}};

const std::array<std::pair<std::string, std::string>, 6> domain_data = {{
    {"Reference Data", "Static reference data including securities, parties, and classifications"},
    {"Market Data", "Real-time and historical pricing, quotes, and market indicators"},
    {"Trade Data", "Transaction records, executions, and settlement information"},
    {"Risk Data", "Risk metrics, exposures, and limit calculations"},
    {"Regulatory Data", "Compliance, reporting, and regulatory submissions"},
    {"Analytics Data", "Derived analytics, models, and computed metrics"}
}};

const std::array<std::pair<std::string, std::string>, 12> subject_area_data = {{
    {"Currencies", "Currency codes and exchange rate data"},
    {"Countries", "Country codes and geographic classifications"},
    {"Securities", "Financial instruments and their characteristics"},
    {"Parties", "Counterparties, issuers, and legal entities"},
    {"Prices", "Asset prices and valuations"},
    {"Quotes", "Bid/ask quotes and market depth"},
    {"Trades", "Executed transactions"},
    {"Positions", "Holdings and portfolio positions"},
    {"Exposures", "Risk exposures and sensitivities"},
    {"Reports", "Regulatory and compliance reports"},
    {"Models", "Quantitative models and analytics"},
    {"Events", "Corporate actions and market events"}
}};

const std::array<std::pair<std::string, std::string>, 5> origin_data = {{
    {"SOURCE", "Primary authoritative source data"},
    {"DERIVED", "Calculated or transformed from other datasets"},
    {"EXTERNAL", "Data obtained from external vendors"},
    {"INTERNAL", "Internally generated data"},
    {"AGGREGATED", "Consolidated from multiple sources"}
}};

const std::array<std::pair<std::string, std::string>, 4> nature_data = {{
    {"ACTUAL", "Real production data from live systems"},
    {"SYNTHETIC", "Generated test data for development"},
    {"HISTORICAL", "Archived data from past periods"},
    {"SIMULATED", "Scenario-based simulation data"}
}};

const std::array<std::pair<std::string, std::string>, 4> treatment_data = {{
    {"RAW", "Unprocessed data as received from source"},
    {"CLEANSED", "Validated and cleaned data"},
    {"ENRICHED", "Data augmented with derived fields"},
    {"AGGREGATED", "Summarized or rolled-up data"}
}};

const std::array<std::string, 20> dataset_names = {{
    "Currency Reference Data",
    "Country Master File",
    "Security Master",
    "Counterparty Registry",
    "Daily Price Feed",
    "Intraday Quotes",
    "Trade Executions",
    "Position Snapshot",
    "Risk Exposures Daily",
    "Regulatory Report Extract",
    "Model Parameters",
    "Corporate Actions Feed",
    "FX Rates Daily",
    "Bond Yields Curve",
    "Equity Indices",
    "Credit Spreads",
    "Volatility Surface",
    "Swap Curves",
    "Client Positions",
    "Margin Calculations"
}};

const std::array<std::string, 10> source_systems = {{
    "BLOOMBERG",
    "REFINITIV",
    "INTERNAL_CALC",
    "TRADE_CAPTURE",
    "RISK_ENGINE",
    "SETTLEMENT_SYS",
    "PRICING_SVC",
    "DATA_WAREHOUSE",
    "REG_REPORTING",
    "ANALYTICS_PLATFORM"
}};

}

domain::synthetic_catalog
catalog_generator_service::generate(const domain::generation_options& options) {
    generation_context ctx(
        options.seed.value_or(std::random_device{}()));

    domain::synthetic_catalog result;
    result.seed = ctx.seed();

    // Generate accounts
    for (std::size_t i = 0; i < options.account_count && i < account_data.size(); ++i) {
        iam::domain::account acc;
        acc.version = 1;
        acc.id = ctx.generate_uuid();
        acc.username = account_data[i].first;
        acc.email = account_data[i].second;
        acc.password_hash = ctx.alphanumeric(64);
        acc.password_salt = ctx.alphanumeric(32);
        acc.totp_secret = ctx.alphanumeric(32);
        acc.modified_by = "system_admin";
        acc.change_reason_code = "INITIAL_SETUP";
        acc.change_commentary = "Initial account creation";
        acc.recorded_at = ctx.past_timepoint();
        result.accounts.push_back(acc);
    }

    auto pick_username = [&]() -> const std::string& {
        return ctx.pick(result.accounts).username;
    };

    // Generate catalogs
    for (std::size_t i = 0; i < options.catalog_count && i < catalog_data.size(); ++i) {
        dq::domain::catalog cat;
        cat.version = 1;
        cat.name = catalog_data[i].first;
        cat.description = catalog_data[i].second;
        if (ctx.random_bool(0.7)) {
            cat.owner = "Data Management Team";
        }
        cat.modified_by = pick_username();
        cat.change_commentary = "Initial catalog setup";
        cat.recorded_at = ctx.past_timepoint();
        result.catalogs.push_back(cat);
    }

    // Generate data domains
    for (std::size_t i = 0; i < options.data_domain_count && i < domain_data.size(); ++i) {
        dq::domain::data_domain dom;
        dom.version = 1;
        dom.name = domain_data[i].first;
        dom.description = domain_data[i].second;
        dom.modified_by = pick_username();
        dom.change_commentary = "Initial domain setup";
        dom.recorded_at = ctx.past_timepoint();
        result.data_domains.push_back(dom);
    }

    // Generate subject areas (linked to domains)
    std::size_t sa_idx = 0;
    for (const auto& dom : result.data_domains) {
        for (std::size_t j = 0; j < options.subject_areas_per_domain &&
             sa_idx < subject_area_data.size(); ++j, ++sa_idx) {
            dq::domain::subject_area sa;
            sa.version = 1;
            sa.name = subject_area_data[sa_idx].first;
            sa.domain_name = dom.name;
            sa.description = subject_area_data[sa_idx].second;
            sa.modified_by = pick_username();
            sa.change_commentary = "Initial subject area setup";
            sa.recorded_at = ctx.past_timepoint();
            result.subject_areas.push_back(sa);
        }
    }

    // Generate origin dimensions
    for (std::size_t i = 0; i < options.origin_dimension_count && i < origin_data.size(); ++i) {
        dq::domain::origin_dimension od;
        od.version = 1;
        od.code = origin_data[i].first;
        od.name = origin_data[i].first;
        od.description = origin_data[i].second;
        od.modified_by = pick_username();
        od.change_commentary = "Initial dimension setup";
        od.recorded_at = ctx.past_timepoint();
        result.origin_dimensions.push_back(od);
    }

    // Generate nature dimensions
    for (std::size_t i = 0; i < options.nature_dimension_count && i < nature_data.size(); ++i) {
        dq::domain::nature_dimension nd;
        nd.version = 1;
        nd.code = nature_data[i].first;
        nd.name = nature_data[i].first;
        nd.description = nature_data[i].second;
        nd.modified_by = pick_username();
        nd.change_commentary = "Initial dimension setup";
        nd.recorded_at = ctx.past_timepoint();
        result.nature_dimensions.push_back(nd);
    }

    // Generate treatment dimensions
    for (std::size_t i = 0; i < options.treatment_dimension_count && i < treatment_data.size(); ++i) {
        dq::domain::treatment_dimension td;
        td.version = 1;
        td.code = treatment_data[i].first;
        td.name = treatment_data[i].first;
        td.description = treatment_data[i].second;
        td.modified_by = pick_username();
        td.change_commentary = "Initial dimension setup";
        td.recorded_at = ctx.past_timepoint();
        result.treatment_dimensions.push_back(td);
    }

    // Generate datasets with proper references
    for (std::size_t i = 0; i < options.dataset_count && i < dataset_names.size(); ++i) {
        dq::domain::dataset ds;
        ds.version = 1;
        ds.id = ctx.generate_uuid();

        // Link to catalog (optional)
        if (ctx.random_bool(0.8) && !result.catalogs.empty()) {
            ds.catalog_name = ctx.pick(result.catalogs).name;
        }

        // Link to subject area and domain
        if (!result.subject_areas.empty()) {
            const auto& sa = ctx.pick(result.subject_areas);
            ds.subject_area_name = sa.name;
            ds.domain_name = sa.domain_name;
        } else if (!result.data_domains.empty()) {
            ds.domain_name = ctx.pick(result.data_domains).name;
            ds.subject_area_name = "General";
        }

        // Link to dimensions
        if (!result.origin_dimensions.empty()) {
            ds.origin_code = ctx.pick(result.origin_dimensions).code;
        }
        if (!result.nature_dimensions.empty()) {
            ds.nature_code = ctx.pick(result.nature_dimensions).code;
        }
        if (!result.treatment_dimensions.empty()) {
            ds.treatment_code = ctx.pick(result.treatment_dimensions).code;
        }

        // Link to methodology if provided
        if (options.methodology_id) {
            ds.methodology_id = *options.methodology_id;
        }

        ds.name = dataset_names[i];
        std::ostringstream desc;
        desc << "Contains " << dataset_names[i] << " for " << ds.domain_name
             << " domain, sourced from " << ds.origin_code << " systems.";
        ds.description = desc.str();

        ds.source_system_id = ctx.pick(source_systems);

        std::ostringstream bctx;
        bctx << "Used for " << ds.subject_area_name << " processing and analysis.";
        ds.business_context = bctx.str();

        ds.lineage_depth = ctx.random_int(0, 3);
        ds.ingestion_timestamp = ctx.past_timepoint(1);
        ds.as_of_date = std::chrono::floor<std::chrono::days>(ds.ingestion_timestamp);

        if (ctx.random_bool(0.3)) {
            ds.license_info = "Internal Use Only";
        }

        ds.modified_by = pick_username();
        ds.change_commentary = "Initial dataset registration";
        ds.recorded_at = ctx.past_timepoint();

        result.datasets.push_back(ds);
    }

    // Stamp dependencies from options onto the result
    result.dependencies = options.dependencies;

    return result;
}

domain::synthetic_catalog catalog_generator_service::generate() {
    return generate(domain::generation_options{});
}

}
