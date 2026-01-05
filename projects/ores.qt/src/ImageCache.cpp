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
#include "ores.qt/ImageCache.hpp"

#include <QtConcurrent>
#include <QSvgRenderer>
#include <QPainter>
#include <QPixmap>
#include "ores.comms/messaging/frame.hpp"
#include "ores.comms/messaging/message_types.hpp"
#include "ores.assets/messaging/assets_protocol.hpp"

namespace ores::qt {

using comms::messaging::frame;
using comms::messaging::message_type;
using namespace ores::telemetry::log;

ImageCache::ImageCache(ClientManager* clientManager, QObject* parent)
    : QObject(parent),
      clientManager_(clientManager),
      mappings_watcher_(new QFutureWatcher<MappingsResult>(this)),
      images_watcher_(new QFutureWatcher<ImagesResult>(this)),
      image_list_watcher_(new QFutureWatcher<ImageListResult>(this)),
      single_image_watcher_(new QFutureWatcher<SingleImageResult>(this)),
      set_currency_image_watcher_(new QFutureWatcher<SetCurrencyImageResult>(this)) {

    connect(mappings_watcher_, &QFutureWatcher<MappingsResult>::finished,
        this, &ImageCache::onMappingsLoaded);
    connect(images_watcher_, &QFutureWatcher<ImagesResult>::finished,
        this, &ImageCache::onImagesLoaded);
    connect(image_list_watcher_, &QFutureWatcher<ImageListResult>::finished,
        this, &ImageCache::onImageListLoaded);
    connect(single_image_watcher_, &QFutureWatcher<SingleImageResult>::finished,
        this, &ImageCache::onSingleImageLoaded);
    connect(set_currency_image_watcher_, &QFutureWatcher<SetCurrencyImageResult>::finished,
        this, &ImageCache::onCurrencyImageSet);
}

void ImageCache::loadCurrencyMappings() {
    if (is_loading_mappings_) {
        BOOST_LOG_SEV(lg(), warn) << "Mappings load already in progress.";
        return;
    }

    if (!clientManager_ || !clientManager_->isConnected()) {
        BOOST_LOG_SEV(lg(), warn) << "Cannot load mappings: not connected.";
        return;
    }

    is_loading_mappings_ = true;
    QPointer<ImageCache> self = this;

    QFuture<MappingsResult> future =
        QtConcurrent::run([self]() -> MappingsResult {
            BOOST_LOG_SEV(lg(), debug) << "Fetching currency-image mappings.";
            if (!self) return {false, {}};

            assets::messaging::get_currency_images_request request;
            auto payload = request.serialize();

            frame request_frame(message_type::get_currency_images_request,
                0, std::move(payload));

            auto response_result =
                self->clientManager_->sendRequest(std::move(request_frame));

            if (!response_result) {
                BOOST_LOG_SEV(lg(), error) << "Failed to send currency images request: "
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
                assets::messaging::get_currency_images_response::deserialize(*payload_result);

            if (!response) {
                BOOST_LOG_SEV(lg(), error) << "Failed to deserialize currency images response.";
                return {false, {}};
            }

            BOOST_LOG_SEV(lg(), debug) << "Received " << response->currency_images.size()
                                       << " currency-image mappings.";

            return {true, std::move(response->currency_images)};
        });

    mappings_watcher_->setFuture(future);
}

void ImageCache::onMappingsLoaded() {
    is_loading_mappings_ = false;

    auto result = mappings_watcher_->result();
    if (result.success) {
        // Store mappings
        currency_to_image_id_.clear();
        for (const auto& mapping : result.mappings) {
            currency_to_image_id_[mapping.iso_code] = mapping.image_id;
        }

        BOOST_LOG_SEV(lg(), info) << "Loaded " << currency_to_image_id_.size()
                                  << " currency-image mappings.";

        emit currencyMappingsLoaded();

        // If loadAll() was called, continue to load images
        if (load_images_after_mappings_) {
            load_images_after_mappings_ = false;
            loadImagesForCurrencies();
        }
    } else {
        BOOST_LOG_SEV(lg(), error) << "Failed to load currency mappings.";
        emit loadError(tr("Failed to load currency-image mappings"));
    }
}

void ImageCache::loadImagesForCurrencies() {
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

    if (image_ids_to_fetch.empty()) {
        BOOST_LOG_SEV(lg(), debug) << "No new images to fetch.";
        emit imagesLoaded();
        emit allLoaded();
        return;
    }

    BOOST_LOG_SEV(lg(), debug) << "Fetching " << image_ids_to_fetch.size() << " images.";

    is_loading_images_ = true;
    QPointer<ImageCache> self = this;

    QFuture<ImagesResult> future =
        QtConcurrent::run([self, image_ids_to_fetch]() -> ImagesResult {
            if (!self) return {false, {}};

            std::vector<assets::domain::image> all_images;

            // Batch into groups of MAX_IMAGES_PER_REQUEST
            constexpr std::size_t batch_size = assets::messaging::MAX_IMAGES_PER_REQUEST;
            for (std::size_t i = 0; i < image_ids_to_fetch.size(); i += batch_size) {
                std::vector<std::string> batch;
                for (std::size_t j = i; j < std::min(i + batch_size, image_ids_to_fetch.size()); ++j) {
                    batch.push_back(image_ids_to_fetch[j]);
                }

                BOOST_LOG_SEV(lg(), debug) << "Fetching batch of " << batch.size() << " images.";

                assets::messaging::get_images_request request;
                request.image_ids = std::move(batch);
                auto payload = request.serialize();

                frame request_frame(message_type::get_images_request,
                    0, std::move(payload));

                auto response_result =
                    self->clientManager_->sendRequest(std::move(request_frame));

                if (!response_result) {
                    BOOST_LOG_SEV(lg(), error) << "Failed to send images request: "
                                               << response_result.error();
                    continue;
                }

                auto payload_result = response_result->decompressed_payload();
                if (!payload_result) {
                    BOOST_LOG_SEV(lg(), error) << "Failed to decompress images response: "
                                               << payload_result.error();
                    continue;
                }

                auto response =
                    assets::messaging::get_images_response::deserialize(*payload_result);

                if (!response) {
                    BOOST_LOG_SEV(lg(), error) << "Failed to deserialize images response.";
                    continue;
                }

                for (auto& img : response->images) {
                    all_images.push_back(std::move(img));
                }
            }

            BOOST_LOG_SEV(lg(), debug) << "Fetched " << all_images.size() << " images total.";
            return {true, std::move(all_images)};
        });

    images_watcher_->setFuture(future);
}

void ImageCache::onImagesLoaded() {
    is_loading_images_ = false;

    auto result = images_watcher_->result();
    if (result.success) {
        // Cache SVG data and render icons
        for (const auto& img : result.images) {
            image_svg_cache_[img.image_id] = img.svg_data;
        }

        // Render icons for all currencies
        for (const auto& [iso_code, image_id] : currency_to_image_id_) {
            auto svg_it = image_svg_cache_.find(image_id);
            if (svg_it != image_svg_cache_.end() &&
                currency_icons_.find(iso_code) == currency_icons_.end()) {
                QIcon icon = svgToIcon(svg_it->second);
                if (!icon.isNull()) {
                    currency_icons_[iso_code] = icon;
                }
            }
        }

        BOOST_LOG_SEV(lg(), info) << "Cached " << currency_icons_.size()
                                  << " currency icons.";

        emit imagesLoaded();
        emit allLoaded();
    } else {
        BOOST_LOG_SEV(lg(), error) << "Failed to load images.";
        emit loadError(tr("Failed to load images"));
    }
}

void ImageCache::loadAll() {
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
    return {};
}

bool ImageCache::hasCurrencyIcon(const std::string& iso_code) const {
    return currency_icons_.find(iso_code) != currency_icons_.end();
}

QIcon ImageCache::svgToIcon(const std::string& svg_data) {
    if (svg_data.empty()) {
        return {};
    }

    QByteArray svgBytes(svg_data.data(), static_cast<qsizetype>(svg_data.size()));
    QSvgRenderer renderer(svgBytes);

    if (!renderer.isValid()) {
        BOOST_LOG_SEV(lg(), warn) << "Invalid SVG data, cannot render icon.";
        return {};
    }

    QIcon icon;

    // Render at multiple sizes for crisp display
    for (int size : {16, 20, 24, 32, 48}) {
        QPixmap pixmap(size, size);
        pixmap.fill(Qt::transparent);

        QPainter painter(&pixmap);
        renderer.render(&painter);
        painter.end();

        icon.addPixmap(pixmap);
    }

    return icon;
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

            assets::messaging::set_currency_image_request request;
            request.iso_code = req_iso_code;
            request.image_id = req_image_id;
            request.assigned_by = req_assigned_by;
            auto payload = request.serialize();

            frame request_frame(message_type::set_currency_image_request,
                0, std::move(payload));

            auto response_result =
                self->clientManager_->sendRequest(std::move(request_frame));

            if (!response_result) {
                BOOST_LOG_SEV(lg(), error) << "Failed to send set currency image request: "
                                           << response_result.error();
                return {false, req_iso_code, "Failed to communicate with server"};
            }

            auto payload_result = response_result->decompressed_payload();
            if (!payload_result) {
                BOOST_LOG_SEV(lg(), error) << "Failed to decompress response: "
                                           << payload_result.error();
                return {false, req_iso_code, "Failed to decompress response"};
            }

            auto response =
                assets::messaging::set_currency_image_response::deserialize(*payload_result);

            if (!response) {
                BOOST_LOG_SEV(lg(), error) << "Failed to deserialize set currency image response.";
                return {false, req_iso_code, "Invalid server response"};
            }

            return {response->success, req_iso_code, response->message};
        });

    set_currency_image_watcher_->setFuture(future);
}

void ImageCache::onCurrencyImageSet() {
    auto result = set_currency_image_watcher_->result();

    if (result.success) {
        BOOST_LOG_SEV(lg(), info) << "Currency image set successfully for: "
                                  << result.iso_code;
        // Reload mappings to get updated data
        loadCurrencyMappings();
    } else {
        BOOST_LOG_SEV(lg(), error) << "Failed to set currency image for "
                                   << result.iso_code << ": " << result.message;
    }

    emit currencyImageSet(QString::fromStdString(result.iso_code),
        result.success, QString::fromStdString(result.message));
}

std::string ImageCache::getCurrencyImageId(const std::string& iso_code) const {
    auto it = currency_to_image_id_.find(iso_code);
    if (it != currency_to_image_id_.end()) {
        return it->second;
    }
    return {};
}

}
