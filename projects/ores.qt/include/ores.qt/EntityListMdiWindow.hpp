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
#ifndef ORES_QT_ENTITY_LIST_MDI_WINDOW_HPP
#define ORES_QT_ENTITY_LIST_MDI_WINDOW_HPP

#include <string_view>
#include <QWidget>
#include <QAction>
#include <QTimer>
#include <QIcon>
#include <QCloseEvent>
#include <QTableView>
#include "ores.logging/make_logger.hpp"

namespace ores::qt {

/**
 * @brief Base class for entity list MDI windows providing stale indicator support.
 *
 * This class provides common functionality for entity list windows including:
 * - Stale indicator support with pulse animation on the refresh action
 * - markAsStale() and clearStaleIndicator() slots for server-side change notifications
 *
 * Subclasses must:
 * 1. Call initializeStaleIndicator() after creating their refresh action
 * 2. Implement reload() to refresh data from the server
 * 3. Call clearStaleIndicator() at the start of their reload() implementation
 *
 * Example usage in subclass:
 * @code
 * void MyEntityMdiWindow::setupToolbar() {
 *     refreshAction_ = toolbar_->addAction(...);
 *     initializeStaleIndicator(refreshAction_, IconUtils::iconPath(Icon::ArrowSync));
 * }
 *
 * void MyEntityMdiWindow::reload() {
 *     clearStaleIndicator();
 *     model_->refresh();
 * }
 * @endcode
 */
class EntityListMdiWindow : public QWidget {
    Q_OBJECT

private:
    inline static std::string_view logger_name = "ores.qt.entity_list_mdi_window";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    explicit EntityListMdiWindow(QWidget* parent = nullptr);
    ~EntityListMdiWindow() override;

    QSize sizeHint() const override;

public slots:
    /**
     * @brief Mark the list as stale (data changed on server).
     *
     * Starts a pulse animation on the refresh action to indicate that
     * server-side data has changed and the list should be reloaded.
     */
    void markAsStale();

    /**
     * @brief Clear the stale indicator.
     *
     * Stops the pulse animation and restores the normal refresh icon.
     * Call this at the start of reload() implementations.
     */
    void clearStaleIndicator();

    /**
     * @brief Reload data from the server.
     *
     * Subclasses must implement this to refresh their data.
     * Implementations should call clearStaleIndicator() first.
     */
    virtual void reload() = 0;

    /**
     * @brief Save window settings (column visibility, window size, etc.).
     *
     * When initializeTableSettings() has been called, this saves header state
     * and window size automatically. Otherwise it is a no-op.
     * Called automatically on close via closeEvent().
     */
    virtual void saveSettings();

protected:
    void closeEvent(QCloseEvent* event) override;

    /**
     * @brief Configure table header, column visibility, and settings persistence.
     *
     * Call once in setupTable() after creating tableView and model.
     * Sets ResizeToContents on all columns, wires up column visibility context
     * menu, restores saved header state and window size.
     *
     * @param tableView       The QTableView to manage
     * @param sourceModel     The source model (for column count/headers)
     * @param settingsGroup   QSettings group name (e.g. "CountryListWindow")
     * @param defaultHiddenColumns  Columns hidden by default when no saved state
     * @param defaultSize     Default window size when no saved size
     * @param settingsVersion Bump when column layout changes to discard old state
     */
    void initializeTableSettings(QTableView* tableView,
                                  QAbstractItemModel* sourceModel,
                                  std::string_view settingsGroup,
                                  const QVector<int>& defaultHiddenColumns = {},
                                  const QSize& defaultSize = {900, 400},
                                  int settingsVersion = 1);

    /**
     * @brief Initialize the stale indicator support.
     *
     * Call this after creating the refresh action in setupToolbar().
     *
     * @param refreshAction The refresh/reload action to animate
     * @param iconPath Path to the refresh icon SVG resource
     */
    void initializeStaleIndicator(QAction* refreshAction, const QString& iconPath);

    /**
     * @brief Get the normal (non-stale) tooltip text for the refresh action.
     *
     * Override this to customize the tooltip text.
     */
    virtual QString normalRefreshTooltip() const { return tr("Refresh"); }

    /**
     * @brief Get the stale tooltip text for the refresh action.
     *
     * Override this to customize the tooltip text.
     */
    virtual QString staleRefreshTooltip() const {
        return tr("Data changed on server - click to reload");
    }

private slots:
    void onPulseTimeout();

private:
    void startPulseAnimation();
    void setupColumnVisibility();
    void showHeaderContextMenu(const QPoint& pos);
    void restoreTableSettings();

    // Stale indicator members
    QAction* refreshAction_{nullptr};
    QTimer* pulseTimer_{nullptr};
    QIcon normalReloadIcon_;
    QIcon pulseReloadIcon_;
    bool pulseState_{false};
    int pulseCount_{0};

    // Table settings members
    QTableView* settingsTableView_{nullptr};
    QAbstractItemModel* settingsModel_{nullptr};
    QString settingsGroup_;
    QVector<int> defaultHiddenColumns_;
    QSize defaultSize_{900, 400};
    int settingsVersion_{1};

protected:
    /**
     * @brief Saved window size from QSettings.
     *
     * Set by initializeTableSettings() when restoring a previously saved window
     * size. Used by sizeHint() to return the saved size instead of the default.
     */
    QSize savedWindowSize_;
};

}

#endif
