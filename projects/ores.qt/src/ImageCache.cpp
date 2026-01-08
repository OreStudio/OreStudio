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
#include "ores.utility/rfl/reflectors.hpp" // IWYU pragma: keep. Must be before rfl/json.hpp
#include "ores.qt/ImageCache.hpp"

#include <algorithm>
#include <iomanip>
#include <sstream>
#include <rfl/json.hpp>
#include <QtConcurrent>
#include <boost/lexical_cast.hpp>
#include <boost/uuid/uuid_io.hpp>
#include "ores.qt/IconUtils.hpp"
#include "ores.comms/messaging/frame.hpp"
#include "ores.comms/messaging/message_types.hpp"
#include "ores.assets/messaging/assets_protocol.hpp"
#include "ores.risk/messaging/currency_protocol.hpp"
#include "ores.risk/messaging/country_protocol.hpp"
#include "ores.risk/eventing/currency_changed_event.hpp"

namespace ores::qt {

using comms::messaging::frame;
using comms::messaging::message_type;
using namespace ores::telemetry::log;

ImageCache::ImageCache(ClientManager* clientManager, QObject* parent)
    : QObject(parent),
      clientManager_(clientManager),
      mappings_watcher_(new QFutureWatcher<MappingsResult>(this)),
      country_mappings_watcher_(new QFutureWatcher<MappingsResult>(this)),
      images_watcher_(new QFutureWatcher<ImagesResult>(this)),
      image_list_watcher_(new QFutureWatcher<ImageListResult>(this)),
      single_image_watcher_(new QFutureWatcher<SingleImageResult>(this)),
      set_currency_image_watcher_(new QFutureWatcher<SetCurrencyImageResult>(this)),
      all_available_watcher_(new QFutureWatcher<ImagesResult>(this)) {

    connect(mappings_watcher_, &QFutureWatcher<MappingsResult>::finished,
        this, &ImageCache::onMappingsLoaded);
    connect(country_mappings_watcher_, &QFutureWatcher<MappingsResult>::finished,
        this, &ImageCache::onCountryMappingsLoaded);
    connect(images_watcher_, &QFutureWatcher<ImagesResult>::finished,
        this, &ImageCache::onImagesLoaded);
    connect(image_list_watcher_, &QFutureWatcher<ImageListResult>::finished,
        this, &ImageCache::onImageListLoaded);
    connect(single_image_watcher_, &QFutureWatcher<SingleImageResult>::finished,
        this, &ImageCache::onSingleImageLoaded);
    connect(set_currency_image_watcher_, &QFutureWatcher<SetCurrencyImageResult>::finished,
        this, &ImageCache::onCurrencyImageSet);
    connect(all_available_watcher_, &QFutureWatcher<ImagesResult>::finished,
        this, &ImageCache::onAllAvailableImagesLoaded);

    // Subscribe to currency change events to invalidate cache when flags change
    const std::string event_name = std::string{
        eventing::domain::event_traits<risk::eventing::currency_changed_event>::name};

    connect(clientManager_, &ClientManager::notificationReceived,
            this, &ImageCache::onNotificationReceived);

    // Subscribe to events when connected
    connect(clientManager_, &ClientManager::connected,
            this, [this, event_name]() {
        BOOST_LOG_SEV(lg(), info) << "Subscribing to currency change events for flag updates";
        clientManager_->subscribeToEvent(event_name);
    });

    // Re-subscribe after reconnection
    connect(clientManager_, &ClientManager::reconnected,
            this, [this, event_name]() {
        BOOST_LOG_SEV(lg(), info) << "Re-subscribing to currency change events after reconnect";
        clientManager_->subscribeToEvent(event_name);
    });

    // If already connected, subscribe now
    if (clientManager_->isConnected()) {
        BOOST_LOG_SEV(lg(), info) << "Already connected, subscribing to currency change events";
        clientManager_->subscribeToEvent(event_name);
    }
}

void ImageCache::loadCurrencyMappings() {
    BOOST_LOG_SEV(lg(), debug) << "loadCurrencyMappings() called.";

    if (is_loading_mappings_) {
        BOOST_LOG_SEV(lg(), warn) << "Mappings load already in progress.";
        return;
    }

    if (!clientManager_) {
        BOOST_LOG_SEV(lg(), error) << "Cannot load mappings: clientManager_ is null.";
        return;
    }

    if (!clientManager_->isConnected()) {
        BOOST_LOG_SEV(lg(), warn) << "Cannot load mappings: not connected.";
        return;
    }

    is_loading_mappings_ = true;
    QPointer<ImageCache> self = this;

    BOOST_LOG_SEV(lg(), debug) << "Starting async currency mappings fetch.";

    QFuture<MappingsResult> future =
        QtConcurrent::run([self]() -> MappingsResult {
            BOOST_LOG_SEV(lg(), debug) << "Fetching currencies to extract image_id mappings.";
            if (!self) {
                BOOST_LOG_SEV(lg(), error) << "ImageCache destroyed during async fetch.";
                return {false, {}};
            }

            // Fetch all currencies (we just need iso_code and image_id)
            risk::messaging::get_currencies_request request;
            request.offset = 0;
            request.limit = 1000;  // Max allowed by server
            auto payload = request.serialize();

            frame request_frame(message_type::get_currencies_request,
                0, std::move(payload));

            BOOST_LOG_SEV(lg(), debug) << "Sending get_currencies_request.";

            auto response_result =
                self->clientManager_->sendRequest(std::move(request_frame));

            if (!response_result) {
                BOOST_LOG_SEV(lg(), error) << "Failed to send currencies request: "
                                           << response_result.error();
                return {false, {}};
            }

            BOOST_LOG_SEV(lg(), debug) << "Received currencies response, decompressing.";

            auto payload_result = response_result->decompressed_payload();
            if (!payload_result) {
                BOOST_LOG_SEV(lg(), error) << "Failed to decompress response: "
                                           << payload_result.error();
                return {false, {}};
            }

            // Log payload size for debugging
            BOOST_LOG_SEV(lg(), debug) << "Payload size: " << payload_result->size() << " bytes.";

            auto response =
                risk::messaging::get_currencies_response::deserialize(*payload_result);

            if (!response) {
                // Log first 64 bytes of payload as hex for debugging
                std::ostringstream hex_dump;
                hex_dump << std::hex << std::setfill('0');
                for (std::size_t i = 0; i < std::min<std::size_t>(64, payload_result->size()); ++i) {
                    hex_dump << std::setw(2) << static_cast<int>((*payload_result)[i]) << " ";
                }
                BOOST_LOG_SEV(lg(), error) << "Failed to deserialize currencies response. "
                                           << "First 64 bytes: " << hex_dump.str();
                return {false, {}};
            }

            BOOST_LOG_SEV(lg(), debug) << "Received " << response->currencies.size()
                                       << " currencies from server.";

            // TEMP: Log first currency as JSON for debugging
            if (!response->currencies.empty()) {
                BOOST_LOG_SEV(lg(), debug) << "First currency JSON: "
                                           << rfl::json::write(response->currencies[0]);
            }

            // Extract iso_code -> image_id mappings from currencies
            std::unordered_map<std::string, std::string> mappings;
            int currencies_with_image = 0;
            int currencies_without_image = 0;
            for (const auto& currency : response->currencies) {
                if (currency.image_id) {
                    std::string image_id_str = boost::uuids::to_string(*currency.image_id);
                    mappings[currency.iso_code] = image_id_str;
                    currencies_with_image++;
                } else {
                    currencies_without_image++;
                }
            }

            BOOST_LOG_SEV(lg(), debug) << "Extracted " << mappings.size()
                                       << " currency-image mappings. "
                                       << currencies_with_image << " currencies have image_id, "
                                       << currencies_without_image << " do not.";

            return {true, std::move(mappings)};
        });

    mappings_watcher_->setFuture(future);
}

void ImageCache::onMappingsLoaded() {
    BOOST_LOG_SEV(lg(), debug) << "onMappingsLoaded() callback triggered.";
    is_loading_mappings_ = false;

    auto result = mappings_watcher_->result();
    BOOST_LOG_SEV(lg(), debug) << "Mappings result: success=" << result.success
                               << ", mappings count=" << result.mappings.size();

    if (result.success) {
        // Store mappings (already in the right format)
        currency_to_image_id_ = std::move(result.mappings);

        BOOST_LOG_SEV(lg(), debug) << "Stored " << currency_to_image_id_.size()
                                   << " currency-image mappings.";

        emit currencyMappingsLoaded();

        // If loadAll() was called, continue to load images
        if (load_images_after_mappings_) {
            load_images_after_mappings_ = false;

            // Check if we're doing a selective refresh for specific currencies
            if (!pending_refresh_iso_codes_.empty()) {
                // Selective refresh: only update the affected currencies
                std::vector<std::string> image_ids_to_fetch;
                std::unordered_set<std::string> unique_ids;

                for (const auto& iso_code : pending_refresh_iso_codes_) {
                    auto it = currency_to_image_id_.find(iso_code);
                    if (it != currency_to_image_id_.end() && !it->second.empty()) {
                        // Only fetch if we don't have this image cached already
                        if (image_svg_cache_.find(it->second) == image_svg_cache_.end() &&
                            unique_ids.find(it->second) == unique_ids.end()) {
                            image_ids_to_fetch.push_back(it->second);
                            unique_ids.insert(it->second);
                        }
                    }
                    // Remove old icon to force re-render
                    currency_icons_.erase(iso_code);
                }

                BOOST_LOG_SEV(lg(), debug) << "Selective refresh: "
                    << pending_refresh_iso_codes_.size() << " currencies, "
                    << image_ids_to_fetch.size() << " new images to fetch.";

                // Re-render icons for affected currencies (using cached images)
                for (const auto& iso_code : pending_refresh_iso_codes_) {
                    auto map_it = currency_to_image_id_.find(iso_code);
                    if (map_it != currency_to_image_id_.end()) {
                        auto svg_it = image_svg_cache_.find(map_it->second);
                        if (svg_it != image_svg_cache_.end()) {
                            QIcon icon = svgToIcon(svg_it->second);
                            if (!icon.isNull()) {
                                currency_icons_[iso_code] = icon;
                            }
                        }
                    }
                }

                pending_refresh_iso_codes_.clear();

                if (image_ids_to_fetch.empty()) {
                    // All images were already cached, we're done
                    emit imagesLoaded();
                    emit allLoaded();
                } else {
                    // Fetch the new images
                    is_loading_images_ = true;
                    ClientManager* clientMgr = clientManager_;
                    QFuture<ImagesResult> future =
                        QtConcurrent::run([clientMgr, image_ids_to_fetch]() -> ImagesResult {
                            return fetchImagesInBatches(clientMgr, image_ids_to_fetch);
                        });
                    images_watcher_->setFuture(future);
                }
            } else {
                // Full refresh: load all images
                loadImagesForCurrencies();
            }
        }
    } else {
        pending_refresh_iso_codes_.clear();
        BOOST_LOG_SEV(lg(), error) << "Failed to load currency mappings.";
        emit loadError(tr("Failed to load currency-image mappings"));
    }
}

void ImageCache::loadImagesForCurrencies() {
    BOOST_LOG_SEV(lg(), debug) << "loadImagesForCurrencies() called. "
                               << "currency_to_image_id_ size: " << currency_to_image_id_.size();

    if (is_loading_images_) {
        BOOST_LOG_SEV(lg(), warn) << "Images load already in progress.";
        return;
    }

    if (!clientManager_ || !clientManager_->isConnected()) {
        BOOST_LOG_SEV(lg(), warn) << "Cannot load images: not connected.";
        return;
    }

    // Collect image IDs that we need to fetch (not already cached)
    std::vector<std::string> image_ids_to_fetch;
    std::unordered_set<std::string> unique_ids;

    for (const auto& [iso_code, image_id] : currency_to_image_id_) {
        if (image_svg_cache_.find(image_id) == image_svg_cache_.end() &&
            unique_ids.find(image_id) == unique_ids.end()) {
            image_ids_to_fetch.push_back(image_id);
            unique_ids.insert(image_id);
        }
    }

    BOOST_LOG_SEV(lg(), debug) << "Need to fetch " << image_ids_to_fetch.size()
                               << " images (cached: " << image_svg_cache_.size() << ").";

    if (image_ids_to_fetch.empty()) {
        BOOST_LOG_SEV(lg(), debug) << "No new images to fetch, re-rendering icons.";

        // Still need to re-render icons for updated mappings using cached SVG data
        for (const auto& [iso_code, image_id] : currency_to_image_id_) {
            auto svg_it = image_svg_cache_.find(image_id);
            if (svg_it != image_svg_cache_.end()) {
                QIcon icon = svgToIcon(svg_it->second);
                if (!icon.isNull()) {
                    currency_icons_[iso_code] = icon;
                }
            }
        }

        emit imagesLoaded();
        emit allLoaded();
        return;
    }

    BOOST_LOG_SEV(lg(), debug) << "Fetching " << image_ids_to_fetch.size() << " images.";

    is_loading_images_ = true;
    ClientManager* clientMgr = clientManager_;

    QFuture<ImagesResult> future =
        QtConcurrent::run([clientMgr, image_ids_to_fetch]() -> ImagesResult {
            return fetchImagesInBatches(clientMgr, image_ids_to_fetch);
        });

    images_watcher_->setFuture(future);
}

void ImageCache::onImagesLoaded() {
    BOOST_LOG_SEV(lg(), debug) << "onImagesLoaded() callback triggered.";
    is_loading_images_ = false;

    auto result = images_watcher_->result();
    BOOST_LOG_SEV(lg(), debug) << "Images result: success=" << result.success
                               << ", images count=" << result.images.size();

    if (result.success) {
        // Cache SVG data and render icons
        for (const auto& img : result.images) {
            const auto image_id_str = boost::uuids::to_string(img.image_id);
            image_svg_cache_[image_id_str] = img.svg_data;
            // Log first 100 chars of SVG to verify content
            std::string svg_preview = img.svg_data.substr(0, 100);
            BOOST_LOG_SEV(lg(), debug) << "Cached SVG for image_id: " << image_id_str
                                       << ", size: " << img.svg_data.size()
                                       << ", preview: " << svg_preview;
        }

        BOOST_LOG_SEV(lg(), debug) << "Cached " << result.images.size() << " SVGs. "
                                   << "Total in cache: " << image_svg_cache_.size();

        // Render icons for all currencies
        int rendered = 0;
        int render_failed = 0;
        int no_svg = 0;
        BOOST_LOG_SEV(lg(), debug) << "Rendering icons for " << currency_to_image_id_.size()
                                   << " currency-to-image mappings.";
        for (const auto& [iso_code, image_id] : currency_to_image_id_) {
            auto svg_it = image_svg_cache_.find(image_id);
            if (svg_it != image_svg_cache_.end()) {
                BOOST_LOG_SEV(lg(), trace) << "Rendering " << iso_code << " with image_id: "
                                           << image_id << ", svg size: " << svg_it->second.size();
                QIcon icon = svgToIcon(svg_it->second);
                if (!icon.isNull()) {
                    // Always update the icon to reflect the latest mapping
                    currency_icons_[iso_code] = icon;
                    rendered++;
                } else {
                    BOOST_LOG_SEV(lg(), warn) << "Failed to render icon for " << iso_code
                                              << " (image_id: " << image_id
                                              << ", svg size: " << svg_it->second.size() << ")";
                    render_failed++;
                }
            } else {
                BOOST_LOG_SEV(lg(), debug) << "No SVG cache for " << iso_code
                                           << " (image_id: " << image_id << ")";
                no_svg++;
            }
        }

        BOOST_LOG_SEV(lg(), debug) << "Rendered " << rendered << " icons, "
                                   << render_failed << " render failures, "
                                   << no_svg << " missing SVGs.";

        BOOST_LOG_SEV(lg(), debug) << "Total currency_icons_: " << currency_icons_.size();

        emit imagesLoaded();
        emit allLoaded();
    } else {
        BOOST_LOG_SEV(lg(), error) << "Failed to load images.";
        emit loadError(tr("Failed to load images"));
    }
}

void ImageCache::loadAll() {
    BOOST_LOG_SEV(lg(), debug) << "loadAll() called.";

    if (is_loading_mappings_ || is_loading_images_) {
        BOOST_LOG_SEV(lg(), warn) << "Load already in progress.";
        return;
    }

    load_images_after_mappings_ = true;
    loadCurrencyMappings();
}

QIcon ImageCache::getCurrencyIcon(const std::string& iso_code) const {
    auto it = currency_icons_.find(iso_code);
    if (it != currency_icons_.end()) {
        return it->second;
    }

    // Return the "no-flag" placeholder icon if available
    return getNoFlagIcon();
}

bool ImageCache::hasCurrencyIcon(const std::string& iso_code) const {
    return currency_icons_.find(iso_code) != currency_icons_.end();
}

QIcon ImageCache::svgToIcon(const std::string& svg_data) {
    return IconUtils::svgDataToIcon(svg_data);
}

ImageCache::ImagesResult ImageCache::fetchImagesInBatches(
    ClientManager* clientManager,
    const std::vector<std::string>& image_ids) {

    BOOST_LOG_SEV(lg(), debug) << "fetchImagesInBatches() called with "
                               << image_ids.size() << " image IDs.";

    if (!clientManager) {
        BOOST_LOG_SEV(lg(), error) << "clientManager is null in fetchImagesInBatches.";
        return {false, {}};
    }

    std::vector<assets::domain::image> all_images;

    // Batch into groups of MAX_IMAGES_PER_REQUEST
    constexpr std::size_t batch_size = assets::messaging::MAX_IMAGES_PER_REQUEST;
    int batch_num = 0;
    for (std::size_t i = 0; i < image_ids.size(); i += batch_size) {
        std::vector<std::string> batch;
        for (std::size_t j = i; j < std::min(i + batch_size, image_ids.size()); ++j) {
            batch.push_back(image_ids[j]);
        }

        batch_num++;
        BOOST_LOG_SEV(lg(), debug) << "Fetching batch " << batch_num << " with "
                                   << batch.size() << " images.";

        assets::messaging::get_images_request request;
        request.image_ids = std::move(batch);
        auto payload = request.serialize();

        frame request_frame(message_type::get_images_request, 0, std::move(payload));

        auto response_result = clientManager->sendRequest(std::move(request_frame));

        if (!response_result) {
            BOOST_LOG_SEV(lg(), error) << "Failed to send images request (batch "
                                       << batch_num << "): " << response_result.error();
            continue;
        }

        auto payload_result = response_result->decompressed_payload();
        if (!payload_result) {
            BOOST_LOG_SEV(lg(), error) << "Failed to decompress images response (batch "
                                       << batch_num << "): " << payload_result.error();
            continue;
        }

        auto response =
            assets::messaging::get_images_response::deserialize(*payload_result);

        if (!response) {
            BOOST_LOG_SEV(lg(), error) << "Failed to deserialize images response (batch "
                                       << batch_num << ").";
            continue;
        }

        BOOST_LOG_SEV(lg(), debug) << "Batch " << batch_num << " returned "
                                   << response->images.size() << " images.";

        for (auto& img : response->images) {
            all_images.push_back(std::move(img));
        }
    }

    BOOST_LOG_SEV(lg(), debug) << "fetchImagesInBatches complete. Total: "
                               << all_images.size() << " images.";
    return {true, std::move(all_images)};
}

void ImageCache::loadImageList() {
    if (!clientManager_ || !clientManager_->isConnected()) {
        BOOST_LOG_SEV(lg(), warn) << "Cannot load image list: not connected.";
        return;
    }

    QPointer<ImageCache> self = this;

    QFuture<ImageListResult> future =
        QtConcurrent::run([self]() -> ImageListResult {
            BOOST_LOG_SEV(lg(), debug) << "Fetching image list.";
            if (!self) return {false, {}};

            assets::messaging::list_images_request request;
            auto payload = request.serialize();

            frame request_frame(message_type::list_images_request,
                0, std::move(payload));

            auto response_result =
                self->clientManager_->sendRequest(std::move(request_frame));

            if (!response_result) {
                BOOST_LOG_SEV(lg(), error) << "Failed to send list images request: "
                                           << response_result.error();
                return {false, {}};
            }

            auto payload_result = response_result->decompressed_payload();
            if (!payload_result) {
                BOOST_LOG_SEV(lg(), error) << "Failed to decompress response: "
                                           << payload_result.error();
                return {false, {}};
            }

            auto response =
                assets::messaging::list_images_response::deserialize(*payload_result);

            if (!response) {
                BOOST_LOG_SEV(lg(), error) << "Failed to deserialize list images response.";
                return {false, {}};
            }

            BOOST_LOG_SEV(lg(), debug) << "Received " << response->images.size()
                                       << " images in list.";

            return {true, std::move(response->images)};
        });

    image_list_watcher_->setFuture(future);
}

void ImageCache::onImageListLoaded() {
    auto result = image_list_watcher_->result();
    if (result.success) {
        available_images_ = std::move(result.images);

        BOOST_LOG_SEV(lg(), info) << "Loaded " << available_images_.size()
                                  << " available images.";

        emit imageListLoaded();
    } else {
        BOOST_LOG_SEV(lg(), error) << "Failed to load image list.";
        emit loadError(tr("Failed to load image list"));
    }
}

void ImageCache::loadImageById(const std::string& image_id) {
    if (!clientManager_ || !clientManager_->isConnected()) {
        BOOST_LOG_SEV(lg(), warn) << "Cannot load image: not connected.";
        return;
    }

    // Check if already cached
    if (image_preview_cache_.find(image_id) != image_preview_cache_.end()) {
        emit imageLoaded(QString::fromStdString(image_id));
        return;
    }

    // Also check in the main SVG cache
    auto svg_it = image_svg_cache_.find(image_id);
    if (svg_it != image_svg_cache_.end()) {
        QIcon icon = svgToIcon(svg_it->second);
        if (!icon.isNull()) {
            image_preview_cache_[image_id] = icon;
            emit imageLoaded(QString::fromStdString(image_id));
        }
        return;
    }

    // Skip if already being loaded
    if (pending_image_requests_.find(image_id) != pending_image_requests_.end()) {
        BOOST_LOG_SEV(lg(), debug) << "Image already pending: " << image_id;
        return;
    }

    pending_image_requests_.insert(image_id);
    QPointer<ImageCache> self = this;
    std::string requested_id = image_id;

    QFuture<SingleImageResult> future =
        QtConcurrent::run([self, requested_id]() -> SingleImageResult {
            BOOST_LOG_SEV(lg(), debug) << "Fetching single image: " << requested_id;
            if (!self) return {false, requested_id, {}};

            assets::messaging::get_images_request request;
            request.image_ids = {requested_id};
            auto payload = request.serialize();

            frame request_frame(message_type::get_images_request,
                0, std::move(payload));

            auto response_result =
                self->clientManager_->sendRequest(std::move(request_frame));

            if (!response_result) {
                BOOST_LOG_SEV(lg(), error) << "Failed to send get image request: "
                                           << response_result.error();
                return {false, requested_id, {}};
            }

            auto payload_result = response_result->decompressed_payload();
            if (!payload_result) {
                BOOST_LOG_SEV(lg(), error) << "Failed to decompress response: "
                                           << payload_result.error();
                return {false, requested_id, {}};
            }

            auto response =
                assets::messaging::get_images_response::deserialize(*payload_result);

            if (!response || response->images.empty()) {
                BOOST_LOG_SEV(lg(), error) << "Failed to get image: " << requested_id;
                return {false, requested_id, {}};
            }

            return {true, requested_id, std::move(response->images[0])};
        });

    single_image_watcher_->setFuture(future);
}

void ImageCache::onSingleImageLoaded() {
    auto result = single_image_watcher_->result();

    // Clear pending status
    pending_image_requests_.erase(result.image_id);

    if (result.success) {
        // Cache SVG and render icon
        image_svg_cache_[result.image_id] = result.image.svg_data;
        QIcon icon = svgToIcon(result.image.svg_data);
        if (!icon.isNull()) {
            image_preview_cache_[result.image_id] = icon;
        }

        BOOST_LOG_SEV(lg(), debug) << "Loaded single image: " << result.image_id;
        emit imageLoaded(QString::fromStdString(result.image_id));
    } else {
        BOOST_LOG_SEV(lg(), error) << "Failed to load image: " << result.image_id;
    }
}

QIcon ImageCache::getImageIcon(const std::string& image_id) const {
    auto it = image_preview_cache_.find(image_id);
    if (it != image_preview_cache_.end()) {
        return it->second;
    }
    return {};
}

void ImageCache::setCurrencyImage(const std::string& iso_code,
    const std::string& image_id, const std::string& assigned_by) {

    if (!clientManager_ || !clientManager_->isConnected()) {
        BOOST_LOG_SEV(lg(), warn) << "Cannot set currency image: not connected.";
        emit currencyImageSet(QString::fromStdString(iso_code), false,
            tr("Not connected to server"));
        return;
    }

    QPointer<ImageCache> self = this;
    std::string req_iso_code = iso_code;
    std::string req_image_id = image_id;
    std::string req_assigned_by = assigned_by;

    QFuture<SetCurrencyImageResult> future =
        QtConcurrent::run([self, req_iso_code, req_image_id, req_assigned_by]() -> SetCurrencyImageResult {
            BOOST_LOG_SEV(lg(), debug) << "Setting currency image: " << req_iso_code
                                       << " -> " << (req_image_id.empty() ? "(none)" : req_image_id);
            if (!self) return {false, req_iso_code, "Widget destroyed"};

            // Step 1: Fetch currencies (protocol doesn't support filtering by iso_code)
            risk::messaging::get_currencies_request get_request;
            get_request.offset = 0;
            get_request.limit = 1000;  // Max allowed by server
            auto get_payload = get_request.serialize();

            frame get_frame(message_type::get_currencies_request, 0, std::move(get_payload));

            auto get_response_result = self->clientManager_->sendRequest(std::move(get_frame));
            if (!get_response_result) {
                BOOST_LOG_SEV(lg(), error) << "Failed to fetch currencies: "
                                           << get_response_result.error();
                return {false, req_iso_code, "Failed to fetch currencies"};
            }

            auto get_payload_result = get_response_result->decompressed_payload();
            if (!get_payload_result) {
                return {false, req_iso_code, "Failed to decompress response"};
            }

            auto get_response =
                risk::messaging::get_currencies_response::deserialize(*get_payload_result);
            if (!get_response) {
                return {false, req_iso_code, "Invalid currencies response"};
            }

            // Find the currency with matching iso_code
            auto it = std::find_if(get_response->currencies.begin(), get_response->currencies.end(),
                [&req_iso_code](const auto& c) { return c.iso_code == req_iso_code; });

            if (it == get_response->currencies.end()) {
                return {false, req_iso_code, "Currency not found"};
            }

            // Step 2: Update the image_id
            auto currency = *it;
            if (req_image_id.empty()) {
                currency.image_id = std::nullopt;
            } else {
                currency.image_id = boost::lexical_cast<boost::uuids::uuid>(req_image_id);
            }
            currency.recorded_by = req_assigned_by;

            // Step 3: Save the updated currency
            risk::messaging::save_currency_request save_request;
            save_request.currency = currency;
            auto save_payload = save_request.serialize();

            frame save_frame(message_type::save_currency_request, 0, std::move(save_payload));

            auto save_response_result = self->clientManager_->sendRequest(std::move(save_frame));
            if (!save_response_result) {
                BOOST_LOG_SEV(lg(), error) << "Failed to save currency: "
                                           << save_response_result.error();
                return {false, req_iso_code, "Failed to save currency"};
            }

            auto save_payload_result = save_response_result->decompressed_payload();
            if (!save_payload_result) {
                return {false, req_iso_code, "Failed to decompress save response"};
            }

            auto save_response =
                risk::messaging::save_currency_response::deserialize(*save_payload_result);
            if (!save_response) {
                return {false, req_iso_code, "Invalid save response"};
            }

            if (save_response->success) {
                BOOST_LOG_SEV(lg(), info) << "Currency image updated: " << req_iso_code
                                          << " -> " << (req_image_id.empty() ? "(none)" : req_image_id);
            }

            return {save_response->success, req_iso_code, save_response->message};
        });

    set_currency_image_watcher_->setFuture(future);
}

void ImageCache::onCurrencyImageSet() {
    auto result = set_currency_image_watcher_->result();

    if (result.success) {
        BOOST_LOG_SEV(lg(), info) << "Currency image set successfully for: "
                                  << result.iso_code;
        // Reload mappings and then images to get updated data
        load_images_after_mappings_ = true;
        loadCurrencyMappings();
    } else {
        BOOST_LOG_SEV(lg(), error) << "Failed to set currency image for "
                                   << result.iso_code << ": " << result.message;
    }

    emit currencyImageSet(QString::fromStdString(result.iso_code),
        result.success, QString::fromStdString(result.message));
}

void ImageCache::loadAllAvailableImages() {
    if (is_loading_all_available_) {
        BOOST_LOG_SEV(lg(), warn) << "All available images load already in progress.";
        return;
    }

    if (!clientManager_ || !clientManager_->isConnected()) {
        BOOST_LOG_SEV(lg(), warn) << "Cannot load all images: not connected.";
        return;
    }

    if (available_images_.empty()) {
        BOOST_LOG_SEV(lg(), warn) << "No images in list to load.";
        emit allAvailableImagesLoaded();
        return;
    }

    // Collect image IDs that we need to fetch (not already cached or pending)
    std::vector<std::string> image_ids_to_fetch;
    for (const auto& img : available_images_) {
        if (image_preview_cache_.find(img.image_id) == image_preview_cache_.end() &&
            image_svg_cache_.find(img.image_id) == image_svg_cache_.end() &&
            pending_image_requests_.find(img.image_id) == pending_image_requests_.end()) {
            image_ids_to_fetch.push_back(img.image_id);
            pending_image_requests_.insert(img.image_id);
        }
    }

    if (image_ids_to_fetch.empty()) {
        BOOST_LOG_SEV(lg(), debug) << "All available images already cached.";
        emit allAvailableImagesLoaded();
        return;
    }

    BOOST_LOG_SEV(lg(), debug) << "Loading " << image_ids_to_fetch.size()
                               << " available images.";

    is_loading_all_available_ = true;
    ClientManager* clientMgr = clientManager_;

    QFuture<ImagesResult> future =
        QtConcurrent::run([clientMgr, image_ids_to_fetch]() -> ImagesResult {
            return fetchImagesInBatches(clientMgr, image_ids_to_fetch);
        });

    all_available_watcher_->setFuture(future);
}

void ImageCache::onAllAvailableImagesLoaded() {
    is_loading_all_available_ = false;

    auto result = all_available_watcher_->result();
    if (result.success) {
        // Cache SVG data and render icons, clear pending status
        for (const auto& img : result.images) {
            const auto image_id_str = boost::uuids::to_string(img.image_id);
            pending_image_requests_.erase(image_id_str);
            image_svg_cache_[image_id_str] = img.svg_data;
            QIcon icon = svgToIcon(img.svg_data);
            if (!icon.isNull()) {
                image_preview_cache_[image_id_str] = icon;
            }
        }

        BOOST_LOG_SEV(lg(), info) << "Cached " << result.images.size()
                                  << " available images.";

        emit allAvailableImagesLoaded();
    } else {
        // Clear pending status for all items that were attempted
        for (const auto& img : available_images_) {
            pending_image_requests_.erase(img.image_id);
        }
        BOOST_LOG_SEV(lg(), error) << "Failed to load all available images.";
        emit loadError(tr("Failed to load available images"));
    }
}

std::string ImageCache::getCurrencyImageId(const std::string& iso_code) const {
    auto it = currency_to_image_id_.find(iso_code);
    if (it != currency_to_image_id_.end()) {
        return it->second;
    }
    return {};
}

std::string ImageCache::getNoFlagImageId() const {
    for (const auto& img : available_images_) {
        if (img.key == "no-flag") {
            return img.image_id;
        }
    }
    BOOST_LOG_SEV(lg(), warn) << "No 'no-flag' image found in available images.";
    return {};
}

QIcon ImageCache::getNoFlagIcon() const {
    std::string no_flag_id = getNoFlagImageId();
    if (no_flag_id.empty()) {
        return {};
    }
    return getImageIcon(no_flag_id);
}

void ImageCache::loadCountryMappings() {
    BOOST_LOG_SEV(lg(), debug) << "loadCountryMappings() called.";

    if (is_loading_country_mappings_) {
        BOOST_LOG_SEV(lg(), warn) << "Country mappings load already in progress.";
        return;
    }

    if (!clientManager_) {
        BOOST_LOG_SEV(lg(), error) << "Cannot load country mappings: clientManager_ is null.";
        return;
    }

    if (!clientManager_->isConnected()) {
        BOOST_LOG_SEV(lg(), warn) << "Cannot load country mappings: not connected.";
        return;
    }

    is_loading_country_mappings_ = true;
    QPointer<ImageCache> self = this;

    BOOST_LOG_SEV(lg(), debug) << "Starting async country mappings fetch.";

    QFuture<MappingsResult> future =
        QtConcurrent::run([self]() -> MappingsResult {
            BOOST_LOG_SEV(lg(), debug) << "Fetching countries to extract image_id mappings.";
            if (!self) {
                BOOST_LOG_SEV(lg(), error) << "ImageCache destroyed during async fetch.";
                return {false, {}};
            }

            // Fetch all countries (we just need alpha2_code and image_id)
            risk::messaging::get_countries_request request;
            request.offset = 0;
            request.limit = 1000;  // Max allowed by server
            auto payload = request.serialize();

            frame request_frame(message_type::get_countries_request,
                0, std::move(payload));

            BOOST_LOG_SEV(lg(), debug) << "Sending get_countries_request.";

            auto response_result =
                self->clientManager_->sendRequest(std::move(request_frame));

            if (!response_result) {
                BOOST_LOG_SEV(lg(), error) << "Failed to send countries request: "
                                           << response_result.error();
                return {false, {}};
            }

            BOOST_LOG_SEV(lg(), debug) << "Received countries response, decompressing.";

            auto payload_result = response_result->decompressed_payload();
            if (!payload_result) {
                BOOST_LOG_SEV(lg(), error) << "Failed to decompress response: "
                                           << payload_result.error();
                return {false, {}};
            }

            BOOST_LOG_SEV(lg(), debug) << "Payload size: " << payload_result->size() << " bytes.";

            auto response =
                risk::messaging::get_countries_response::deserialize(*payload_result);

            if (!response) {
                BOOST_LOG_SEV(lg(), error) << "Failed to deserialize countries response.";
                return {false, {}};
            }

            BOOST_LOG_SEV(lg(), debug) << "Received " << response->countries.size()
                                       << " countries from server.";

            // Extract alpha2_code -> image_id mappings from countries
            std::unordered_map<std::string, std::string> mappings;
            int countries_with_image = 0;
            int countries_without_image = 0;
            for (const auto& country : response->countries) {
                if (country.image_id) {
                    std::string image_id_str = boost::uuids::to_string(*country.image_id);
                    mappings[country.alpha2_code] = image_id_str;
                    countries_with_image++;
                } else {
                    countries_without_image++;
                }
            }

            BOOST_LOG_SEV(lg(), debug) << "Extracted " << mappings.size()
                                       << " country-image mappings. "
                                       << countries_with_image << " countries have image_id, "
                                       << countries_without_image << " do not.";

            return {true, std::move(mappings)};
        });

    country_mappings_watcher_->setFuture(future);
}

void ImageCache::onCountryMappingsLoaded() {
    BOOST_LOG_SEV(lg(), debug) << "onCountryMappingsLoaded() callback triggered.";
    is_loading_country_mappings_ = false;

    auto result = country_mappings_watcher_->result();
    BOOST_LOG_SEV(lg(), debug) << "Country mappings result: success=" << result.success
                               << ", mappings count=" << result.mappings.size();

    if (result.success) {
        country_to_image_id_ = std::move(result.mappings);

        BOOST_LOG_SEV(lg(), debug) << "Stored " << country_to_image_id_.size()
                                   << " country-image mappings.";

        emit countryMappingsLoaded();
    } else {
        BOOST_LOG_SEV(lg(), error) << "Failed to load country mappings.";
        emit loadError(tr("Failed to load country-image mappings"));
    }
}

void ImageCache::loadImagesForCountries() {
    BOOST_LOG_SEV(lg(), debug) << "loadImagesForCountries() called. "
                               << "country_to_image_id_ size: " << country_to_image_id_.size();

    if (!clientManager_ || !clientManager_->isConnected()) {
        BOOST_LOG_SEV(lg(), warn) << "Cannot load country images: not connected.";
        return;
    }

    // Collect image IDs that we need to fetch (not already cached)
    std::vector<std::string> image_ids_to_fetch;
    std::unordered_set<std::string> unique_ids;

    for (const auto& [alpha2_code, image_id] : country_to_image_id_) {
        if (image_svg_cache_.find(image_id) == image_svg_cache_.end() &&
            unique_ids.find(image_id) == unique_ids.end()) {
            image_ids_to_fetch.push_back(image_id);
            unique_ids.insert(image_id);
        }
    }

    BOOST_LOG_SEV(lg(), debug) << "Need to fetch " << image_ids_to_fetch.size()
                               << " country images (cached: " << image_svg_cache_.size() << ").";

    // First, render icons using any already-cached SVG data
    int rendered = 0;
    for (const auto& [alpha2_code, image_id] : country_to_image_id_) {
        auto svg_it = image_svg_cache_.find(image_id);
        if (svg_it != image_svg_cache_.end()) {
            QIcon icon = svgToIcon(svg_it->second);
            if (!icon.isNull()) {
                country_icons_[alpha2_code] = icon;
                rendered++;
            }
        }
    }

    BOOST_LOG_SEV(lg(), debug) << "Rendered " << rendered << " country icons from cache.";

    if (image_ids_to_fetch.empty()) {
        BOOST_LOG_SEV(lg(), debug) << "No new country images to fetch.";
        emit imagesLoaded();
        return;
    }

    // Fetch missing images synchronously in this call (countries list is small)
    // and render icons immediately
    BOOST_LOG_SEV(lg(), debug) << "Fetching " << image_ids_to_fetch.size() << " country images.";

    ClientManager* clientMgr = clientManager_;
    auto result = fetchImagesInBatches(clientMgr, image_ids_to_fetch);

    if (result.success) {
        // Cache SVG data and render icons
        for (const auto& img : result.images) {
            const auto image_id_str = boost::uuids::to_string(img.image_id);
            image_svg_cache_[image_id_str] = img.svg_data;
        }

        // Render icons for all countries
        for (const auto& [alpha2_code, image_id] : country_to_image_id_) {
            if (country_icons_.find(alpha2_code) != country_icons_.end()) {
                continue;  // Already rendered from cache above
            }
            auto svg_it = image_svg_cache_.find(image_id);
            if (svg_it != image_svg_cache_.end()) {
                QIcon icon = svgToIcon(svg_it->second);
                if (!icon.isNull()) {
                    country_icons_[alpha2_code] = icon;
                }
            }
        }

        BOOST_LOG_SEV(lg(), debug) << "Total country icons: " << country_icons_.size();
        emit imagesLoaded();
    } else {
        BOOST_LOG_SEV(lg(), error) << "Failed to load country images.";
        emit loadError(tr("Failed to load country images"));
    }
}

QIcon ImageCache::getCountryIcon(const std::string& alpha2_code) const {
    auto it = country_icons_.find(alpha2_code);
    if (it != country_icons_.end()) {
        return it->second;
    }

    // Return the "no-flag" placeholder icon if available
    return getNoFlagIcon();
}

bool ImageCache::hasCountryIcon(const std::string& alpha2_code) const {
    return country_icons_.find(alpha2_code) != country_icons_.end();
}

void ImageCache::onNotificationReceived(const QString& eventType, const QDateTime& timestamp,
                                         const QStringList& entityIds) {
    static const std::string event_name = std::string{
        eventing::domain::event_traits<risk::eventing::currency_changed_event>::name};
    if (eventType.toStdString() != event_name) {
        return;
    }

    BOOST_LOG_SEV(lg(), info) << "Currency changed event received at "
                              << timestamp.toString(Qt::ISODate).toStdString()
                              << " for " << entityIds.size() << " currencies.";

    if (entityIds.isEmpty()) {
        // No specific currencies provided, reload everything
        loadAll();
        return;
    }

    // Store the ISO codes for selective refresh after mappings reload
    pending_refresh_iso_codes_.clear();
    for (const QString& id : entityIds) {
        pending_refresh_iso_codes_.push_back(id.toStdString());
    }

    // Reload mappings; onMappingsLoaded will selectively update only affected currencies
    load_images_after_mappings_ = true;
    loadCurrencyMappings();
}

}
