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
#ifndef ORES_SYNTHETIC_SERVICE_SHARED_SURFACES_HPP
#define ORES_SYNTHETIC_SERVICE_SHARED_SURFACES_HPP

#include "ores.marketdata.api/domain/i_feed.hpp"
#include "ores.synthetic.api/feeds/fx_vol_surface_feed.hpp"
#include <cstdint>
#include <functional>
#include <map>
#include <memory>
#include <mutex>
#include <stdexcept>
#include <string>
#include <utility>
#include <vector>

namespace ores::synthetic::service {

/**
 * @brief The latent volatility surfaces the adapters share, one per surface feed.
 *
 * Each adapter runs its own generators, but the contributors of a surface observe one market: a
 * surface started once for each vendor would be a latent path for each, and the vendors would
 * quote different markets. So a surface is held here, by its feed, started when the first adapter
 * attaches to it and stopped when the last one detaches.
 */
class shared_surfaces final {
public:
    using surface_ptr = std::shared_ptr<feed::fx_vol_surface_feed>;

    /**
     * @brief Attaches an adapter to a surface, building and starting the surface first if no
     * adapter holds it, and answers the surface.
     */
    surface_ptr attach(const std::string& feed_id,
                       const std::string& adapter_id,
                       const std::string& contributor,
                       feed::fx_vol_surface_feed::point_targets targets,
                       const std::function<surface_ptr()>& make) {
        const std::lock_guard lock(mu_);
        auto found = surfaces_.find(feed_id);
        if (found == surfaces_.end()) {
            auto surface = make();
            if (!surface)
                throw std::runtime_error("the surface of feed " + feed_id + " was not built");
            surface->start();
            found = surfaces_.emplace(feed_id, std::move(surface)).first;
        }
        found->second->attach(adapter_id, contributor, std::move(targets));
        return found->second;
    }

    /// Detaches an adapter from a surface, and stops the surface when no adapter holds it.
    void detach(const std::string& feed_id, const std::string& adapter_id) {
        const std::lock_guard lock(mu_);
        const auto found = surfaces_.find(feed_id);
        if (found == surfaces_.end())
            return;
        if (found->second->detach(adapter_id) == 0) {
            found->second->stop();
            surfaces_.erase(found);
        }
    }

private:
    std::mutex mu_;
    std::map<std::string, surface_ptr> surfaces_;
};

/**
 * @brief An adapter's hold on a shared surface, as the generator it runs.
 *
 * The runner starts and stops it as it would any generator; starting attaches the adapter's
 * contributor to the surface and stopping detaches it. Its name is the surface's with the
 * contributor's provider, so each adapter's hold runs under a name of its own.
 */
class shared_surface_hold final : public ores::marketdata::domain::i_feed {
public:
    shared_surface_hold(shared_surfaces& surfaces,
                        std::string feed_id,
                        std::string adapter_id,
                        std::string contributor,
                        std::string source_name,
                        feed::fx_vol_surface_feed::point_targets targets,
                        std::function<shared_surfaces::surface_ptr()> make)
        : surfaces_(surfaces)
        , feed_id_(std::move(feed_id))
        , adapter_id_(std::move(adapter_id))
        , contributor_(std::move(contributor))
        , source_name_(std::move(source_name))
        , targets_(std::move(targets))
        , make_(std::move(make)) {}

    ~shared_surface_hold() override {
        stop();
    }

    const std::string& source_name() const override {
        return source_name_;
    }
    const std::vector<std::string>& series_uris() const override {
        return surface_ ? surface_->series_uris() : empty_;
    }
    const std::string& role() const override {
        return role_;
    }
    const std::string& nats_subject() const override {
        return surface_ ? surface_->nats_subject() : empty_name_;
    }
    std::string_view kind() const override {
        return feed::fx_vol_surface_feed_kind;
    }
    void start() override {
        if (!surface_)
            surface_ = surfaces_.attach(feed_id_, adapter_id_, contributor_, targets_, make_);
    }
    void stop() override {
        if (surface_) {
            surfaces_.detach(feed_id_, adapter_id_);
            surface_.reset();
        }
    }
    std::uint64_t publish_count() const override {
        return surface_ ? surface_->publish_count() : 0;
    }

private:
    shared_surfaces& surfaces_;
    std::string feed_id_;
    std::string adapter_id_;
    std::string contributor_;
    std::string source_name_;
    feed::fx_vol_surface_feed::point_targets targets_;
    std::function<shared_surfaces::surface_ptr()> make_;
    shared_surfaces::surface_ptr surface_;
    std::string role_ = "fx_vol_surface";
    std::string empty_name_;
    std::vector<std::string> empty_;
};

}

#endif
