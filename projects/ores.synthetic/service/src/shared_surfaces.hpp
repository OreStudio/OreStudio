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
#include <cstddef>
#include <cstdint>
#include <functional>
#include <map>
#include <memory>
#include <mutex>
#include <stdexcept>
#include <string>
#include <string_view>
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
template <typename Surface>
class basic_shared_surfaces final {
public:
    using surface_ptr = std::shared_ptr<Surface>;
    using point_targets = typename Surface::point_targets;

    /**
     * @brief Attaches an adapter to a surface, building and starting the surface first if no
     * adapter holds it, and answers the surface.
     *
     * The adapter is attached before the surface starts, so no tick of a surface the adapters
     * hold goes out in the form of one no adapter holds.
     */
    surface_ptr attach(const std::string& feed_id,
                       const std::string& adapter_id,
                       const std::string& contributor,
                       point_targets targets,
                       const std::function<surface_ptr()>& make) {
        const std::lock_guard lock(mu_);
        if (const auto found = surfaces_.find(feed_id); found != surfaces_.end()) {
            found->second->attach(adapter_id, contributor, std::move(targets));
            return found->second;
        }
        auto surface = make();
        if (!surface)
            throw std::runtime_error("the surface of feed " + feed_id + " was not built");
        surface->attach(adapter_id, contributor, std::move(targets));
        surface->start();
        surfaces_.emplace(feed_id, surface);
        return surface;
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

    /// How many surfaces run.
    [[nodiscard]] std::size_t running_count() const {
        const std::lock_guard lock(mu_);
        return surfaces_.size();
    }

private:
    mutable std::mutex mu_;
    std::map<std::string, surface_ptr> surfaces_;
};

using shared_surfaces = basic_shared_surfaces<feed::fx_vol_surface_feed>;

/**
 * @brief An adapter's hold on a shared surface, as the generator it runs.
 *
 * The runner starts and stops it as it would any generator; starting attaches the adapter's
 * contributor to the surface and stopping detaches it. Its name is the surface's with the
 * contributor's provider, so each adapter's hold runs under a name of its own. The runner starts
 * it on a thread of its own and stops it from another, so the surface it holds is guarded.
 */
template <typename Surface>
class basic_shared_surface_hold final : public ores::marketdata::domain::i_feed {
public:
    using surfaces_type = basic_shared_surfaces<Surface>;

    basic_shared_surface_hold(surfaces_type& surfaces,
                              std::string feed_id,
                              std::string adapter_id,
                              std::string contributor,
                              std::string source_name,
                              typename surfaces_type::point_targets targets,
                              std::function<typename surfaces_type::surface_ptr()> make)
        : surfaces_(surfaces)
        , feed_id_(std::move(feed_id))
        , adapter_id_(std::move(adapter_id))
        , contributor_(std::move(contributor))
        , source_name_(std::move(source_name))
        , targets_(std::move(targets))
        , make_(std::move(make)) {}

    ~basic_shared_surface_hold() override {
        stop();
    }

    basic_shared_surface_hold(const basic_shared_surface_hold&) = delete;
    basic_shared_surface_hold& operator=(const basic_shared_surface_hold&) = delete;

    const std::string& source_name() const override {
        return source_name_;
    }
    const std::vector<std::string>& series_uris() const override {
        return empty_;
    }
    const std::string& role() const override {
        return role_;
    }
    const std::string& nats_subject() const override {
        return empty_name_;
    }
    std::string_view kind() const override {
        return feed::fx_vol_surface_feed_kind;
    }
    void start() override {
        const std::lock_guard lock(mu_);
        if (!surface_)
            surface_ = surfaces_.attach(feed_id_, adapter_id_, contributor_, targets_, make_);
    }
    void stop() override {
        const std::lock_guard lock(mu_);
        if (surface_) {
            surfaces_.detach(feed_id_, adapter_id_);
            surface_.reset();
        }
    }
    std::uint64_t publish_count() const override {
        const std::lock_guard lock(mu_);
        return surface_ ? surface_->publish_count() : 0;
    }

private:
    surfaces_type& surfaces_;
    std::string feed_id_;
    std::string adapter_id_;
    std::string contributor_;
    std::string source_name_;
    typename surfaces_type::point_targets targets_;
    std::function<typename surfaces_type::surface_ptr()> make_;
    mutable std::mutex mu_;
    typename surfaces_type::surface_ptr surface_;
    std::string role_ = "fx_vol_surface";
    std::string empty_name_;
    std::vector<std::string> empty_;
};

using shared_surface_hold = basic_shared_surface_hold<feed::fx_vol_surface_feed>;

}

#endif
