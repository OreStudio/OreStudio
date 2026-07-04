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
#ifndef ORES_QT_ABSTRACT_CLIENT_MODEL_HPP
#define ORES_QT_ABSTRACT_CLIENT_MODEL_HPP

#include "ores.qt/ImageCache.hpp"
#include "ores.qt/WorkspaceContext.hpp"
#include "ores.qt/export.hpp"
#include <QAbstractTableModel>
#include <QString>
#include <boost/uuid/uuid.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <optional>
#include <vector>

namespace ores::qt {

/**
 * @brief Base class for all client-side entity models.
 *
 * Declares the standard loading lifecycle signals that EntityListMdiWindow
 * connects to in order to manage the loading indicator and reload button
 * state automatically. Subclasses must emit these signals when an async
 * fetch completes or fails.
 */
class ORES_QT_API AbstractClientModel : public QAbstractTableModel {
    Q_OBJECT

public:
    using QAbstractTableModel::QAbstractTableModel;

    /**
     * @brief Set the workspace context for this window's data requests.
     *
     * When set to anything other than the Live sentinel, all fetch operations
     * will use this workspace id in X-Workspace-Id instead of the shared
     * ClientManager workspace context. Call before or instead of reload().
     */
    void setWorkspaceContext(const WorkspaceContext& ctx) {
        workspace_ctx_ = ctx;
    }

    /**
     * @brief Get the current per-window workspace context.
     */
    const WorkspaceContext& workspaceContext() const {
        return workspace_ctx_;
    }

    /**
     * @brief Inject the shared image cache for flag/icon decoration.
     *
     * Controllers call this on models with one or more icon-bearing columns.
     * Once set, the model refreshes every such column whenever the cache
     * loads. Derived classes declare their icon column(s) via iconColumn()
     * (single, image_id-keyed — flagDecoration()) and/or iconColumns()
     * (any number, any accessor — e.g. a currency-code-keyed flag lookup with
     * no image_id involved). Models with neither leave both at their
     * defaults (-1 / empty), so there is no icon behaviour.
     */
    void setImageCache(ImageCache* cache) {
        imageCache_ = cache;
        if (!imageCache_)
            return;
        const auto refresh = [this]() {
            std::vector<int> cols = iconColumns();
            if (const int col = iconColumn(); col >= 0)
                cols.push_back(col);
            if (cols.empty())
                return;
            const int rows = rowCount();
            if (rows == 0)
                return;
            for (int col : cols)
                emit dataChanged(index(0, col), index(rows - 1, col), {Qt::DecorationRole});
        };
        connect(imageCache_, &ImageCache::imagesLoaded, this, refresh);
        connect(imageCache_, &ImageCache::allLoaded, this, refresh);
        connect(
            imageCache_, &ImageCache::imageLoaded, this, [refresh](const QString&) { refresh(); });
    }

signals:
    void dataLoaded();
    void loadError(const QString& error_message, const QString& details = {});

protected:
    /**
     * @brief Column that renders the flag/icon decoration; -1 disables it.
     *
     * Entities with a flag override this to return the model column whose
     * Qt::DecorationRole should show the flag. Default -1 = no icon column.
     */
    virtual int iconColumn() const {
        return -1;
    }

    /**
     * @brief Columns that render an icon decoration via any accessor, not
     * just the single image_id-keyed one iconColumn()/flagDecoration()
     * cover — e.g. a currency-code-keyed flag (ImageCache::
     * getCurrencyFlagIcon()) or a composited two-field icon
     * (currency_flag_icon()). Default empty = no such columns.
     */
    virtual std::vector<int> iconColumns() const {
        return {};
    }

    /**
     * @brief Flag icon for a row's image_id, or the "no flag" placeholder.
     *
     * Generic over entity type: keyed purely on image_id. Returns an empty
     * QVariant when no cache is set. Call from a derived data() override for
     * the Qt::DecorationRole on iconColumn().
     */
    QVariant flagDecoration(const std::optional<boost::uuids::uuid>& image_id) const {
        if (!imageCache_)
            return {};
        if (image_id)
            return imageCache_->getIcon(boost::uuids::to_string(*image_id));
        return imageCache_->getNoFlagIcon();
    }

    ImageCache* imageCache_ = nullptr;

private:
    WorkspaceContext workspace_ctx_;
};

}

#endif
