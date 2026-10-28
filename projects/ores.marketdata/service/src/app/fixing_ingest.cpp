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
#include "ores.marketdata.service/app/fixing_ingest.hpp"
#include "ores.marketdata.api/domain/market_fixing.hpp"
#include "ores.marketdata.core/datum/ore_index_codec.hpp"
#include "ores.marketdata.core/datum/oresmd_uri_codec.hpp"
#include "ores.marketdata.core/repository/market_fixing_repository.hpp"
#include "ores.marketdata.core/repository/market_series_repository.hpp"
#include "ores.marketdata.core/repository/series_classification_rule_repository.hpp"
#include "ores.marketdata.service/app/feed_series.hpp"
#include <boost/uuid/uuid_io.hpp>
#include <stdexcept>
#include <utility>

namespace ores::marketdata::service::app {

using namespace ores::logging;

namespace {

/// The series type and metric an index series is classified under, as the import of ORE
/// fixings classifies it.
constexpr std::string_view fixing_series_type = "FIXING";
constexpr std::string_view fixing_metric = "RATE";

/// How many fixings the thread takes from the queue at once.
constexpr std::size_t batch_limit = 2048;

}

bool adds_to(const stored_fixings& stored, const fixing& f) {
    const auto found = stored.find(f.date);
    return found == stored.end() || found->second != f.value;
}

fixing_store::fixing_store(ores::database::context ctx)
    : ctx_(std::move(ctx)) {}

boost::uuids::uuid fixing_store::series_for(const ores::database::context& tenant_ctx,
                                            const consumer& owner,
                                            const std::string& index_uri) {
    const auto party = boost::uuids::to_string(owner.party_id);
    const series_key key{
        owner.tenant_id.to_string(), owner.party_id, owner.provider_code, index_uri};
    if (const auto found = series_.find(key); found != series_.end())
        return found->second;

    repository::market_series_repository series_repo;
    auto existing =
        series_repo.read_latest_by_key(tenant_ctx, index_uri, owner.provider_code, party);
    if (existing.empty()) {
        const auto index = datum::oresmd_uri_codec::read_index(index_uri);
        if (!index)
            throw std::runtime_error("'" + index_uri + "' names no index: " + index.error());
        auto& classifier = classifiers_[owner.tenant_id.to_string()];
        if (!classifier)
            classifier = std::make_shared<const core::series_classifier>(
                repository::series_classification_rule_repository{}.read_latest(tenant_ctx));
        const auto cl = classifier->classify(std::string(fixing_series_type),
                                             std::string(fixing_metric),
                                             datum::ore_index_codec::write(*index));
        BOOST_LOG_SEV(lg(), info) << "Creating fixing series " << index_uri << " for provider "
                                  << owner.provider_code;
        existing.push_back(create_feed_series(
            tenant_ctx, owner, uuid_gen_(), index_uri, cl, ctx_.service_account()));
    }
    const auto id = existing.front().id;
    series_.emplace(key, id);
    return id;
}

stored_fixings& fixing_store::stored_in(const ores::database::context& tenant_ctx,
                                        const boost::uuids::uuid& series_id) {
    auto [it, fresh] = stored_.try_emplace(series_id);
    if (fresh)
        for (auto& row :
             repository::market_fixing_repository{}.read_latest_for_series(tenant_ctx, series_id))
            it->second.insert_or_assign(row.fixing_date, std::move(row.value));
    return it->second;
}

std::size_t fixing_store::store(std::span<const consumer> consumers,
                                std::span<const fixing> fixings) {
    std::size_t written = 0;
    for (const auto& owner : consumers) {
        const auto tenant_ctx = ctx_.with_tenant(owner.tenant_id, "ores.marketdata.service");
        std::vector<domain::market_fixing> rows;
        for (const auto& f : fixings) {
            const auto series_id = series_for(tenant_ctx, owner, f.index_uri);
            auto& stored = stored_in(tenant_ctx, series_id);
            if (!adds_to(stored, f))
                continue;
            domain::market_fixing row;
            row.id = uuid_gen_();
            row.tenant_id = owner.tenant_id;
            row.party_id = owner.party_id;
            row.series_id = series_id;
            row.fixing_date = f.date;
            row.value = f.value;
            row.source = f.source;
            rows.push_back(std::move(row));
            stored.insert_or_assign(f.date, f.value);
        }
        if (rows.empty())
            continue;
        try {
            repository::market_fixing_repository{}.insert(tenant_ctx, rows);
        } catch (...) {
            // What the store refused is not held, so it is tried again when sent again.
            for (const auto& row : rows)
                stored_.erase(row.series_id);
            throw;
        }
        written += rows.size();
    }
    return written;
}

fixing_ingest::fixing_ingest(ores::database::context ctx, consumers_of consumers)
    : store_(std::move(ctx))
    , consumers_(std::move(consumers))
    , thread_([this] { run(); }) {}

fixing_ingest::~fixing_ingest() {
    {
        std::lock_guard lock(mu_);
        stopping_ = true;
    }
    ready_.notify_all();
    if (thread_.joinable())
        thread_.join();
}

void fixing_ingest::push(fixing f) {
    {
        std::lock_guard lock(mu_);
        pending_.push_back(std::move(f));
    }
    ready_.notify_one();
}

std::uint64_t fixing_ingest::stored() const {
    return stored_.load(std::memory_order_relaxed);
}

std::uint64_t fixing_ingest::failed() const {
    return failed_.load(std::memory_order_relaxed);
}

std::size_t fixing_ingest::pending() const {
    std::lock_guard lock(mu_);
    return pending_.size();
}

void fixing_ingest::run() {
    for (;;) {
        std::vector<fixing> batch;
        {
            std::unique_lock lock(mu_);
            ready_.wait(lock, [this] { return stopping_ || !pending_.empty(); });
            if (pending_.empty())
                return;
            const auto n = std::min(pending_.size(), batch_limit);
            batch.assign(std::make_move_iterator(pending_.begin()),
                         std::make_move_iterator(pending_.begin() + static_cast<long>(n)));
            pending_.erase(pending_.begin(), pending_.begin() + static_cast<long>(n));
        }

        std::map<std::string, std::vector<fixing>> by_source;
        for (auto& f : batch)
            by_source[f.source].push_back(std::move(f));
        for (const auto& [source, fixings] : by_source) {
            try {
                stored_.fetch_add(store_.store(consumers_(source), fixings),
                                  std::memory_order_relaxed);
            } catch (const std::exception& e) {
                failed_.fetch_add(fixings.size(), std::memory_order_relaxed);
                BOOST_LOG_SEV(lg(), error)
                    << "Failed to store " << fixings.size() << " fixing(s) of source '" << source
                    << "': " << e.what();
            }
        }
    }
}

}
