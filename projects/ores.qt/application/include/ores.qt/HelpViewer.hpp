/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*-
 *
 * Copyright (C) 2024-2026 Marco Craveiro <marco.craveiro@gmail.com>
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
 * MA 02110-1301, USA.
 *
 */
#ifndef ORES_QT_HELP_VIEWER_HPP
#define ORES_QT_HELP_VIEWER_HPP

#include "ores.logging/make_logger.hpp"
#include <QString>
#include <QWidget>
#include <optional>

class QHelpEngine;
class QLineEdit;

namespace ores::qt {

/**
 * @brief In-application user-manual viewer backed by the Qt help system.
 *
 * Loads the user_manual.qch help collection (built from the org-mode user
 * manual via deploy_help_qch) through a QHelpEngine and presents it with a
 * sidebar — contents tree, keyword index and full-text search — alongside a
 * content pane. Designed to be embedded in an MDI subwindow like AboutDialog.
 *
 * The content pane is a QTextBrowser subclass that resolves qthelp:// URLs
 * against the QHelpEngine, so links, images and cross-references inside the
 * collection render without touching the filesystem or the network.
 */
class HelpViewer final : public QWidget {
    Q_OBJECT

private:
    inline static std::string_view logger_name = "ores.qt.help_viewer";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    explicit HelpViewer(QWidget* parent = nullptr);
    ~HelpViewer() override;

    /**
     * @brief Locate the user_manual.qch shipped with the application.
     *
     * Searches, in order: an explicit ORES_HELP_QCH environment override,
     * locations relative to the running executable (install layouts), and
     * the in-tree build output (build/output/help). Returns std::nullopt
     * when no collection can be found.
     */
    [[nodiscard]] static std::optional<QString> locateHelpCollection();

    /// True when a help collection was found and registered.
    [[nodiscard]] bool isAvailable() const;

private:
    /// Build the sidebar (contents / index / search) and content pane.
    void buildUi();
    /// Run the current full-text search query.
    void runSearch();

    QHelpEngine* engine_{nullptr};
    QLineEdit* searchField_{nullptr};
    bool available_{false};
};

}

#endif
