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
#ifndef ORES_QT_CHANGE_REASON_CACHE_HPP
#define ORES_QT_CHANGE_REASON_CACHE_HPP

#include <vector>
#include <unordered_map>
#include <QObject>
#include <QFutureWatcher>
#include "ores.qt/ClientManager.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.iam/domain/change_reason.hpp"
#include "ores.iam/domain/change_reason_category.hpp"

namespace ores::qt {

/**
 * @brief Shared cache for change reasons used across all entity dialogs.
 *
 * This class manages fetching and caching of change reasons from the server.
 * It listens for change_reason_changed events to automatically refresh the
 * cache when reasons are modified.
 *
 * Typical usage:
 * 1. Create once in MainWindow, pass to dialogs that need it
 * 2. Call loadAll() after connection to preload reasons
 * 3. Use getReasonsForAmend/getReasonsForDelete to get filtered lists
 * 4. Cache automatically refreshes on server events
 */
class ChangeReasonCache final : public QObject {
    Q_OBJECT

private:
    inline static std::string_view logger_name = "ores.qt.change_reason_cache";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    explicit ChangeReasonCache(ClientManager* clientManager,
        QObject* parent = nullptr);
    ~ChangeReasonCache() override = default;

    /**
     * @brief Load all change reasons and categories from the server.
     *
     * This should be called after successful connection. The cache will
     * automatically refresh when change_reason_changed events are received.
     */
    void loadAll();

    /**
     * @brief Check if reasons have been loaded.
     */
    bool isLoaded() const { return is_loaded_; }

    /**
     * @brief Check if loading is in progress.
     */
    bool isLoading() const { return is_loading_; }

    /**
     * @brief Get all change reasons.
     */
    const std::vector<iam::domain::change_reason>& allReasons() const {
        return reasons_;
    }

    /**
     * @brief Get change reasons filtered for amend operations.
     *
     * @param category_code Filter by category (e.g., "common")
     * @return Reasons where applies_to_amend is true, sorted by display_order
     */
    std::vector<iam::domain::change_reason> getReasonsForAmend(
        const std::string& category_code) const;

    /**
     * @brief Get change reasons filtered for delete operations.
     *
     * @param category_code Filter by category (e.g., "common")
     * @return Reasons where applies_to_delete is true, sorted by display_order
     */
    std::vector<iam::domain::change_reason> getReasonsForDelete(
        const std::string& category_code) const;

    /**
     * @brief Get a reason by its code.
     *
     * @param code The reason code (e.g., "common.rectification")
     * @return Pointer to reason if found, nullptr otherwise
     */
    const iam::domain::change_reason* getReasonByCode(
        const std::string& code) const;

    /**
     * @brief Check if a reason code is valid.
     *
     * @param code The reason code to validate
     * @return true if the code exists in the cache
     */
    bool isValidReasonCode(const std::string& code) const;

    /**
     * @brief Get all change reason categories.
     */
    const std::vector<iam::domain::change_reason_category>& allCategories() const {
        return categories_;
    }

signals:
    /**
     * @brief Emitted when reasons and categories have been loaded.
     */
    void loaded();

    /**
     * @brief Emitted when an error occurs during loading.
     */
    void loadError(const QString& error_message);

    /**
     * @brief Emitted when the cache is refreshed due to server event.
     */
    void refreshed();

private slots:
    void onReasonsLoaded();
    void onCategoriesLoaded();
    void onNotificationReceived(const QString& eventType,
        const QDateTime& timestamp, const QStringList& entityIds);

private:
    void loadReasons();
    void loadCategories();
    void subscribeToEvents();

    struct ReasonsResult {
        bool success;
        std::vector<iam::domain::change_reason> reasons;
    };

    struct CategoriesResult {
        bool success;
        std::vector<iam::domain::change_reason_category> categories;
    };

    ClientManager* clientManager_;

    // Cached data
    std::vector<iam::domain::change_reason> reasons_;
    std::vector<iam::domain::change_reason_category> categories_;

    // Index for fast lookup by code
    std::unordered_map<std::string, std::size_t> reason_index_;

    // Loading state
    bool is_loaded_{false};
    bool is_loading_{false};
    bool reasons_loaded_{false};
    bool categories_loaded_{false};

    QFutureWatcher<ReasonsResult>* reasons_watcher_;
    QFutureWatcher<CategoriesResult>* categories_watcher_;
};

}

#endif
