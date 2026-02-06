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
#include <sstream>
#include <rfl/json.hpp>
#include "ores.platform/time/datetime.hpp"
#include <QTimer>
#include <QtConcurrent>
#include <boost/lexical_cast.hpp>
#include <boost/uuid/uuid_io.hpp>
#include "ores.qt/IconUtils.hpp"
#include "ores.comms/messaging/frame.hpp"
#include "ores.comms/messaging/message_type.hpp"
#include "ores.assets/messaging/assets_protocol.hpp"
#include "ores.refdata/messaging/currency_protocol.hpp"
#include "ores.refdata/messaging/country_protocol.hpp"

namespace ores::qt {

using comms::messaging::frame;
using comms::messaging::message_type;
using namespace ores::logging;

ImageCache::ImageCache(ClientManager* clientManager, QObject* parent)
    : QObject(parent),
      clientManager_(clientManager),
      currency_ids_watcher_(new QFutureWatcher<ImageIdsResult>(this)),
      country_ids_watcher_(new QFutureWatcher<ImageIdsResult>(this)),
      incremental_changes_watcher_(new QFutureWatcher<ImageIdsResult>(this)),
      images_watcher_(new QFutureWatcher<ImagesResult>(this)),
      image_list_watcher_(new QFutureWatcher<ImageListResult>(this)),
      single_image_watcher_(new QFutureWatcher<SingleImageResult>(this)),
      set_currency_image_watcher_(new QFutureWatcher<SetCurrencyImageResult>(this)),
      set_country_image_watcher_(new QFutureWatcher<SetCountryImageResult>(this)),
      all_available_watcher_(new QFutureWatcher<ImagesResult>(this)) {

    connect(currency_ids_watcher_, &QFutureWatcher<ImageIdsResult>::finished,
        this, &ImageCache::onCurrencyImageIdsLoaded);
    connect(country_ids_watcher_, &QFutureWatcher<ImageIdsResult>::finished,
        this, &ImageCache::onCountryImageIdsLoaded);
    connect(incremental_changes_watcher_, &QFutureWatcher<ImageIdsResult>::finished,
        this, &ImageCache::onIncrementalChangesLoaded);
    connect(images_watcher_, &QFutureWatcher<ImagesResult>::finished,
        this, &ImageCache::onImagesLoaded);
    connect(image_list_watcher_, &QFutureWatcher<ImageListResult>::finished,
        this, &ImageCache::onImageListLoaded);
    connect(single_image_watcher_, &QFutureWatcher<SingleImageResult>::finished,
        this, &ImageCache::onSingleImageLoaded);
    connect(set_currency_image_watcher_, &QFutureWatcher<SetCurrencyImageResult>::finished,
        this, &ImageCache::onCurrencyImageSet);
    connect(set_country_image_watcher_, &QFutureWatcher<SetCountryImageResult>::finished,
        this, &ImageCache::onCountryImageSet);
    connect(all_available_watcher_, &QFutureWatcher<ImagesResult>::finished,
        this, &ImageCache::onAllAvailableImagesLoaded);
}

void ImageCache::loadAll() {
    BOOST_LOG_SEV(lg(), debug) << "loadAll() called.";

    if (load_all_in_progress_ || is_loading_images_) {
        BOOST_LOG_SEV(lg(), warn) << "Load already in progress.";
        return;
    }

    load_all_in_progress_ = true;
    pending_image_ids_.clear();

    // Start by loading currency image IDs
    loadCurrencyImageIds();
}

void ImageCache::reload() {
    BOOST_LOG_SEV(lg(), info) << "reload() called."
                              << " load_all_in_progress=" << load_all_in_progress_
                              << " is_loading_images=" << is_loading_images_
                              << " is_loading_all_available=" << is_loading_all_available_
                              << " has_last_load_time=" << last_load_time_.has_value();

    // Wait for any in-progress loads to settle
    if (load_all_in_progress_ || is_loading_images_ || is_loading_all_available_) {
        BOOST_LOG_SEV(lg(), warn) << "Load in progress, will retry reload.";
        // Schedule a retry after a short delay
        QTimer::singleShot(500, this, &ImageCache::reload);
        return;
    }

    // If we have a last load time, do incremental reload
    if (last_load_time_) {
        BOOST_LOG_SEV(lg(), info) << "Performing incremental reload since last load time.";
        loadIncrementalChanges();
        return;
    }

    // First time load - clear caches and do full load
    BOOST_LOG_SEV(lg(), info) << "No last load time, performing full reload.";
    const auto svg_count = image_svg_cache_.size();
    const auto icon_count = image_icons_.size();
    image_svg_cache_.clear();
    image_icons_.clear();
    pending_image_ids_.clear();
    pending_image_requests_.clear();
    available_images_.clear();

    BOOST_LOG_SEV(lg(), debug) << "Caches cleared (was: svg=" << svg_count
                               << " icons=" << icon_count << "), starting loadAll().";

    // Reload everything
    loadAll();
}

void ImageCache::clear() {
    BOOST_LOG_SEV(lg(), info) << "clear() called - resetting all caches and load state.";

    image_svg_cache_.clear();
    image_icons_.clear();
    pending_image_ids_.clear();
    pending_image_requests_.clear();
    // Don't clear available_images_ - it's metadata used for placeholder lookup
    // and will be refreshed by loadImageList() if needed
    last_load_time_.reset();

    BOOST_LOG_SEV(lg(), debug) << "Caches cleared and last_load_time reset.";
}

void ImageCache::loadCurrencyImageIds() {
    BOOST_LOG_SEV(lg(), debug) << "loadCurrencyImageIds() called.";

    if (!clientManager_ || !clientManager_->isConnected()) {
        BOOST_LOG_SEV(lg(), warn) << "Cannot load currency image IDs: not connected.";
        load_all_in_progress_ = false;
        return;
    }

    QPointer<ImageCache> self = this;

    QFuture<ImageIdsResult> future =
        QtConcurrent::run([self]() -> ImageIdsResult {
            BOOST_LOG_SEV(lg(), debug) << "Fetching currencies to extract image_ids.";
            if (!self) {
                BOOST_LOG_SEV(lg(), error) << "ImageCache destroyed during async fetch.";
                return {false, {}};
            }

            refdata::messaging::get_currencies_request request;
            request.offset = 0;
            request.limit = 1000;
            auto payload = request.serialize();

            frame request_frame(message_type::get_currencies_request, 0, std::move(payload));

            auto response_result = self->clientManager_->sendRequest(std::move(request_frame));

            if (!response_result) {
                BOOST_LOG_SEV(lg(), error) << "Failed to send currencies request: "
                                           << response_result.error();
                return {false, {}};
            }

            auto payload_result = response_result->decompressed_payload();
            if (!payload_result) {
                BOOST_LOG_SEV(lg(), error) << "Failed to decompress response: "
                                           << payload_result.error();
                return {false, {}};
            }

            auto response = refdata::messaging::get_currencies_response::deserialize(*payload_result);
            if (!response) {
                BOOST_LOG_SEV(lg(), error) << "Failed to deserialize currencies response.";
                return {false, {}};
            }

            BOOST_LOG_SEV(lg(), debug) << "Received " << response->currencies.size()
                                       << " currencies from server.";

            std::vector<std::string> image_ids;
            for (const auto& currency : response->currencies) {
                if (currency.image_id) {
                    image_ids.push_back(boost::uuids::to_string(*currency.image_id));
                }
            }

            BOOST_LOG_SEV(lg(), debug) << "Extracted " << image_ids.size()
                                       << " image IDs from currencies.";

            return {true, std::move(image_ids)};
        });

    currency_ids_watcher_->setFuture(future);
}

void ImageCache::onCurrencyImageIdsLoaded() {
    BOOST_LOG_SEV(lg(), debug) << "onCurrencyImageIdsLoaded() callback triggered.";

    auto result = currency_ids_watcher_->result();
    if (result.success) {
        // Add to pending list (will deduplicate later)
        for (const auto& id : result.image_ids) {
            pending_image_ids_.push_back(id);
        }
        BOOST_LOG_SEV(lg(), debug) << "Added " << result.image_ids.size()
                                   << " currency image IDs. Total pending: "
                                   << pending_image_ids_.size();
    } else {
        BOOST_LOG_SEV(lg(), error) << "Failed to load currency image IDs.";
    }

    // Continue to load country image IDs
    loadCountryImageIds();
}

void ImageCache::loadCountryImageIds() {
    BOOST_LOG_SEV(lg(), debug) << "loadCountryImageIds() called.";

    if (!clientManager_ || !clientManager_->isConnected()) {
        BOOST_LOG_SEV(lg(), warn) << "Cannot load country image IDs: not connected.";
        // Still try to load what we have from currencies
        loadImagesByIds(pending_image_ids_);
        return;
    }

    QPointer<ImageCache> self = this;

    QFuture<ImageIdsResult> future =
        QtConcurrent::run([self]() -> ImageIdsResult {
            BOOST_LOG_SEV(lg(), debug) << "Fetching countries to extract image_ids.";
            if (!self) {
                BOOST_LOG_SEV(lg(), error) << "ImageCache destroyed during async fetch.";
                return {false, {}};
            }

            refdata::messaging::get_countries_request request;
            request.offset = 0;
            request.limit = 1000;
            auto payload = request.serialize();

            frame request_frame(message_type::get_countries_request, 0, std::move(payload));

            auto response_result = self->clientManager_->sendRequest(std::move(request_frame));

            if (!response_result) {
                BOOST_LOG_SEV(lg(), error) << "Failed to send countries request: "
                                           << response_result.error();
                return {false, {}};
            }

            auto payload_result = response_result->decompressed_payload();
            if (!payload_result) {
                BOOST_LOG_SEV(lg(), error) << "Failed to decompress response: "
                                           << payload_result.error();
                return {false, {}};
            }

            auto response = refdata::messaging::get_countries_response::deserialize(*payload_result);
            if (!response) {
                BOOST_LOG_SEV(lg(), error) << "Failed to deserialize countries response.";
                return {false, {}};
            }

            BOOST_LOG_SEV(lg(), debug) << "Received " << response->countries.size()
                                       << " countries from server.";

            std::vector<std::string> image_ids;
            for (const auto& country : response->countries) {
                if (country.image_id) {
                    image_ids.push_back(boost::uuids::to_string(*country.image_id));
                }
            }

            BOOST_LOG_SEV(lg(), debug) << "Extracted " << image_ids.size()
                                       << " image IDs from countries.";

            return {true, std::move(image_ids)};
        });

    country_ids_watcher_->setFuture(future);
}

void ImageCache::onCountryImageIdsLoaded() {
    BOOST_LOG_SEV(lg(), debug) << "onCountryImageIdsLoaded() callback triggered.";

    auto result = country_ids_watcher_->result();
    if (result.success) {
        for (const auto& id : result.image_ids) {
            pending_image_ids_.push_back(id);
        }
        BOOST_LOG_SEV(lg(), debug) << "Added " << result.image_ids.size()
                                   << " country image IDs. Total pending: "
                                   << pending_image_ids_.size();
    } else {
        BOOST_LOG_SEV(lg(), error) << "Failed to load country image IDs.";
    }

    // Now load all the images
    loadImagesByIds(pending_image_ids_);
}

void ImageCache::loadImagesByIds(const std::vector<std::string>& image_ids) {
    BOOST_LOG_SEV(lg(), debug) << "loadImagesByIds() called with "
                               << image_ids.size() << " IDs.";

    // Deduplicate and filter out already-cached images
    std::vector<std::string> ids_to_fetch;
    std::unordered_set<std::string> seen;

    for (const auto& id : image_ids) {
        if (seen.find(id) == seen.end() &&
            image_svg_cache_.find(id) == image_svg_cache_.end()) {
            ids_to_fetch.push_back(id);
            seen.insert(id);
        }
    }

    BOOST_LOG_SEV(lg(), debug) << "Need to fetch " << ids_to_fetch.size()
                               << " images (already cached: " << image_svg_cache_.size() << ").";

    if (ids_to_fetch.empty()) {
        BOOST_LOG_SEV(lg(), debug) << "All images already cached, no changes.";
        load_all_in_progress_ = false;
        // Don't emit allLoaded - nothing changed, no need to refresh UI
        return;
    }

    is_loading_images_ = true;
    ClientManager* clientMgr = clientManager_;

    QFuture<ImagesResult> future =
        QtConcurrent::run([clientMgr, ids_to_fetch]() -> ImagesResult {
            return fetchImagesInBatches(clientMgr, ids_to_fetch);
        });

    images_watcher_->setFuture(future);
}

void ImageCache::loadIncrementalChanges() {
    BOOST_LOG_SEV(lg(), debug) << "loadIncrementalChanges() called.";

    if (!clientManager_ || !clientManager_->isConnected()) {
        BOOST_LOG_SEV(lg(), warn) << "Cannot load incremental changes: not connected.";
        return;
    }

    if (!last_load_time_) {
        BOOST_LOG_SEV(lg(), warn) << "No last load time, falling back to full reload.";
        last_load_time_ = std::nullopt;  // Force full reload
        reload();
        return;
    }

    // Format last load time for logging (thread-safe)
    const auto time_str =
        platform::time::datetime::format_time_point_utc(*last_load_time_) + " UTC";
    BOOST_LOG_SEV(lg(), info) << "Incremental reload: fetching images modified since "
                              << time_str;

    load_all_in_progress_ = true;
    QPointer<ImageCache> self = this;
    auto modified_since = *last_load_time_;

    QFuture<ImageIdsResult> future =
        QtConcurrent::run([self, modified_since]() -> ImageIdsResult {
            BOOST_LOG_SEV(lg(), debug) << "Fetching images modified since last load.";
            if (!self) {
                BOOST_LOG_SEV(lg(), error) << "ImageCache destroyed during async fetch.";
                return {.success = false, .image_ids = {}};
            }

            // Build request with modified_since filter
            assets::messaging::list_images_request request;
            request.modified_since = modified_since;
            auto payload = request.serialize();

            frame request_frame(message_type::list_images_request, 0, std::move(payload));

            auto response_result = self->clientManager_->sendRequest(std::move(request_frame));

            if (!response_result) {
                BOOST_LOG_SEV(lg(), error) << "Failed to send list images request: "
                                           << response_result.error();
                return {.success = false, .image_ids = {}};
            }

            auto payload_result = response_result->decompressed_payload();
            if (!payload_result) {
                BOOST_LOG_SEV(lg(), error) << "Failed to decompress response: "
                                           << payload_result.error();
                return {.success = false, .image_ids = {}};
            }

            auto response = assets::messaging::list_images_response::deserialize(*payload_result);
            if (!response) {
                BOOST_LOG_SEV(lg(), error) << "Failed to deserialize list images response.";
                return {.success = false, .image_ids = {}};
            }

            BOOST_LOG_SEV(lg(), debug) << "Received " << response->images.size()
                                       << " images modified since last load.";

            std::vector<std::string> image_ids;
            for (const auto& img : response->images) {
                image_ids.push_back(img.image_id);
            }

            return {.success = true, .image_ids = std::move(image_ids)};
        });

    incremental_changes_watcher_->setFuture(future);
}

void ImageCache::onIncrementalChangesLoaded() {
    BOOST_LOG_SEV(lg(), debug) << "onIncrementalChangesLoaded() callback triggered.";

    auto result = incremental_changes_watcher_->result();

    if (!result.success) {
        BOOST_LOG_SEV(lg(), error) << "Failed to fetch incremental image changes.";
        load_all_in_progress_ = false;
        return;
    }

    if (result.image_ids.empty()) {
        BOOST_LOG_SEV(lg(), info) << "No images changed since last load.";
        load_all_in_progress_ = false;
        return;
    }

    BOOST_LOG_SEV(lg(), info) << "Loading " << result.image_ids.size()
                              << " changed images.";
    loadImagesByIds(result.image_ids);
}

void ImageCache::onImagesLoaded() {
    BOOST_LOG_SEV(lg(), debug) << "onImagesLoaded() callback triggered.";
    is_loading_images_ = false;
    load_all_in_progress_ = false;
    pending_image_ids_.clear();

    auto result = images_watcher_->result();
    BOOST_LOG_SEV(lg(), debug) << "Images result: success=" << result.success
                               << ", images count=" << result.images.size()
                               << ", failed_batches=" << result.failed_batches;

    if (result.success) {
        // Cache SVG data and render icons
        for (const auto& img : result.images) {
            const auto image_id_str = boost::uuids::to_string(img.image_id);
            image_svg_cache_[image_id_str] = img.svg_data;

            QIcon icon = svgToIcon(img.svg_data);
            if (!icon.isNull()) {
                image_icons_[image_id_str] = icon;
            }
        }

        // Record successful load time for future incremental loads
        last_load_time_ = std::chrono::system_clock::now();
        const auto time_str =
            platform::time::datetime::format_time_point_utc(*last_load_time_) + " UTC";
        BOOST_LOG_SEV(lg(), debug) << "Recorded last load time: " << time_str;

        BOOST_LOG_SEV(lg(), info) << "Cached " << result.images.size() << " images. "
                                  << "Total icons: " << image_icons_.size();

        // Only emit if we actually loaded something
        if (!result.images.empty()) {
            emit imagesLoaded();
            emit allLoaded();
        } else {
            BOOST_LOG_SEV(lg(), debug) << "No images loaded, skipping UI refresh signals.";
        }

        // Notify user if some batches failed (e.g., due to CRC errors)
        if (result.failed_batches > 0) {
            BOOST_LOG_SEV(lg(), warn) << result.failed_batches << " image batches failed to load";
            emit loadError(tr("Some images could not be loaded due to a transmission error. "
                             "Try refreshing to reload missing images."));
        }
    } else {
        BOOST_LOG_SEV(lg(), error) << "Failed to load images.";
        emit loadError(tr("Failed to load images"));
    }
}

QIcon ImageCache::getIcon(const std::string& image_id) {
    if (image_id.empty()) {
        return getNoFlagIcon();
    }

    // Check if already cached
    auto it = image_icons_.find(image_id);
    if (it != image_icons_.end()) {
        return it->second;
    }

    // Not cached - trigger async load if not already loading
    if (pending_image_requests_.find(image_id) == pending_image_requests_.end()) {
        loadImageById(image_id);
    }

    // Return placeholder while loading
    return getNoFlagIcon();
}

bool ImageCache::hasIcon(const std::string& image_id) const {
    return image_icons_.find(image_id) != image_icons_.end();
}

void ImageCache::loadImageById(const std::string& image_id) {
    if (!clientManager_ || !clientManager_->isConnected()) {
        BOOST_LOG_SEV(lg(), warn) << "Cannot load image: not connected.";
        return;
    }

    // Check if already cached in SVG cache
    auto svg_it = image_svg_cache_.find(image_id);
    if (svg_it != image_svg_cache_.end()) {
        QIcon icon = svgToIcon(svg_it->second);
        if (!icon.isNull()) {
            image_icons_[image_id] = icon;
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

            frame request_frame(message_type::get_images_request, 0, std::move(payload));

            auto response_result = self->clientManager_->sendRequest(std::move(request_frame));

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

            auto response = assets::messaging::get_images_response::deserialize(*payload_result);

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
            image_icons_[result.image_id] = icon;
        }

        BOOST_LOG_SEV(lg(), debug) << "Loaded single image: " << result.image_id;
        emit imageLoaded(QString::fromStdString(result.image_id));
    } else {
        BOOST_LOG_SEV(lg(), error) << "Failed to load image: " << result.image_id;
    }
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
        return {.success = false, .images = {}, .failed_batches = 0};
    }

    std::vector<assets::domain::image> all_images;
    int failed_batches = 0;

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
            ++failed_batches;
            continue;
        }

        auto payload_result = response_result->decompressed_payload();
        if (!payload_result) {
            BOOST_LOG_SEV(lg(), error) << "Failed to decompress images response (batch "
                                       << batch_num << "): " << payload_result.error();
            ++failed_batches;
            continue;
        }

        auto response = assets::messaging::get_images_response::deserialize(*payload_result);

        if (!response) {
            BOOST_LOG_SEV(lg(), error) << "Failed to deserialize images response (batch "
                                       << batch_num << ").";
            ++failed_batches;
            continue;
        }

        BOOST_LOG_SEV(lg(), debug) << "Batch " << batch_num << " returned "
                                   << response->images.size() << " images.";

        for (auto& img : response->images) {
            all_images.push_back(std::move(img));
        }
    }

    BOOST_LOG_SEV(lg(), debug) << "fetchImagesInBatches complete. Total: "
                               << all_images.size() << " images, failed_batches: "
                               << failed_batches;
    return {.success = true, .images = std::move(all_images), .failed_batches = failed_batches};
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

            frame request_frame(message_type::list_images_request, 0, std::move(payload));

            auto response_result = self->clientManager_->sendRequest(std::move(request_frame));

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

            auto response = assets::messaging::list_images_response::deserialize(*payload_result);

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

    // Collect image IDs that we need to fetch (not already cached)
    std::vector<std::string> image_ids_to_fetch;
    for (const auto& img : available_images_) {
        if (image_svg_cache_.find(img.image_id) == image_svg_cache_.end() &&
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
        // Cache SVG data and render icons
        for (const auto& img : result.images) {
            const auto image_id_str = boost::uuids::to_string(img.image_id);
            pending_image_requests_.erase(image_id_str);
            image_svg_cache_[image_id_str] = img.svg_data;
            QIcon icon = svgToIcon(img.svg_data);
            if (!icon.isNull()) {
                image_icons_[image_id_str] = icon;
            }
        }

        BOOST_LOG_SEV(lg(), info) << "Cached " << result.images.size()
                                  << " available images.";

        emit allAvailableImagesLoaded();
    } else {
        for (const auto& img : available_images_) {
            pending_image_requests_.erase(img.image_id);
        }
        BOOST_LOG_SEV(lg(), error) << "Failed to load all available images.";
        emit loadError(tr("Failed to load available images"));
    }
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
    auto it = image_icons_.find(no_flag_id);
    if (it != image_icons_.end()) {
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

            // Step 1: Fetch currencies
            refdata::messaging::get_currencies_request get_request;
            get_request.offset = 0;
            get_request.limit = 1000;
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
                refdata::messaging::get_currencies_response::deserialize(*get_payload_result);
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
            refdata::messaging::save_currency_request save_request;
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
                refdata::messaging::save_currency_response::deserialize(*save_payload_result);
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
    } else {
        BOOST_LOG_SEV(lg(), error) << "Failed to set currency image for "
                                   << result.iso_code << ": " << result.message;
    }

    emit currencyImageSet(QString::fromStdString(result.iso_code),
        result.success, QString::fromStdString(result.message));
}

void ImageCache::setCountryImage(const std::string& alpha2_code,
    const std::string& image_id, const std::string& assigned_by) {

    if (!clientManager_ || !clientManager_->isConnected()) {
        BOOST_LOG_SEV(lg(), warn) << "Cannot set country image: not connected.";
        emit countryImageSet(QString::fromStdString(alpha2_code), false,
            tr("Not connected to server"));
        return;
    }

    QPointer<ImageCache> self = this;
    std::string req_alpha2_code = alpha2_code;
    std::string req_image_id = image_id;
    std::string req_assigned_by = assigned_by;

    QFuture<SetCountryImageResult> future =
        QtConcurrent::run([self, req_alpha2_code, req_image_id, req_assigned_by]() -> SetCountryImageResult {
            BOOST_LOG_SEV(lg(), debug) << "Setting country image: " << req_alpha2_code
                                       << " -> " << (req_image_id.empty() ? "(none)" : req_image_id);
            if (!self) return {false, req_alpha2_code, "Widget destroyed"};

            // Step 1: Fetch countries
            refdata::messaging::get_countries_request get_request;
            get_request.offset = 0;
            get_request.limit = 1000;
            auto get_payload = get_request.serialize();

            frame get_frame(message_type::get_countries_request, 0, std::move(get_payload));

            auto get_response_result = self->clientManager_->sendRequest(std::move(get_frame));
            if (!get_response_result) {
                BOOST_LOG_SEV(lg(), error) << "Failed to fetch countries: "
                                           << get_response_result.error();
                return {false, req_alpha2_code, "Failed to fetch countries"};
            }

            auto get_payload_result = get_response_result->decompressed_payload();
            if (!get_payload_result) {
                return {false, req_alpha2_code, "Failed to decompress response"};
            }

            auto get_response =
                refdata::messaging::get_countries_response::deserialize(*get_payload_result);
            if (!get_response) {
                return {false, req_alpha2_code, "Invalid countries response"};
            }

            // Find the country with matching alpha2_code
            auto it = std::find_if(get_response->countries.begin(), get_response->countries.end(),
                [&req_alpha2_code](const auto& c) { return c.alpha2_code == req_alpha2_code; });

            if (it == get_response->countries.end()) {
                return {false, req_alpha2_code, "Country not found"};
            }

            // Step 2: Update the image_id
            auto country = *it;
            if (req_image_id.empty()) {
                country.image_id = std::nullopt;
            } else {
                country.image_id = boost::lexical_cast<boost::uuids::uuid>(req_image_id);
            }
            country.recorded_by = req_assigned_by;

            // Step 3: Save the updated country
            refdata::messaging::save_country_request save_request;
            save_request.country = country;
            auto save_payload = save_request.serialize();

            frame save_frame(message_type::save_country_request, 0, std::move(save_payload));

            auto save_response_result = self->clientManager_->sendRequest(std::move(save_frame));
            if (!save_response_result) {
                BOOST_LOG_SEV(lg(), error) << "Failed to save country: "
                                           << save_response_result.error();
                return {false, req_alpha2_code, "Failed to save country"};
            }

            auto save_payload_result = save_response_result->decompressed_payload();
            if (!save_payload_result) {
                return {false, req_alpha2_code, "Failed to decompress save response"};
            }

            auto save_response =
                refdata::messaging::save_country_response::deserialize(*save_payload_result);
            if (!save_response) {
                return {false, req_alpha2_code, "Invalid save response"};
            }

            if (save_response->success) {
                BOOST_LOG_SEV(lg(), info) << "Country image updated: " << req_alpha2_code
                                          << " -> " << (req_image_id.empty() ? "(none)" : req_image_id);
            }

            return {save_response->success, req_alpha2_code, save_response->message};
        });

    set_country_image_watcher_->setFuture(future);
}

void ImageCache::onCountryImageSet() {
    auto result = set_country_image_watcher_->result();

    if (result.success) {
        BOOST_LOG_SEV(lg(), info) << "Country image set successfully for: "
                                  << result.alpha2_code;
    } else {
        BOOST_LOG_SEV(lg(), error) << "Failed to set country image for "
                                   << result.alpha2_code << ": " << result.message;
    }

    emit countryImageSet(QString::fromStdString(result.alpha2_code),
        result.success, QString::fromStdString(result.message));
}

}
