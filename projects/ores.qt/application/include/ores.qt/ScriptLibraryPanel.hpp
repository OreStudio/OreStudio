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
#ifndef ORES_QT_SCRIPT_LIBRARY_PANEL_HPP
#define ORES_QT_SCRIPT_LIBRARY_PANEL_HPP

#include "ores.logging/make_logger.hpp"
#include <QLineEdit>
#include <QString>
#include <QTreeWidget>
#include <QWidget>

namespace ores::qt {

/**
 * @brief Browse the ores-shell script library.
 *
 * A pure browser: a filterable tree of scripts from two sources.
 * /Library/ scripts are the shipped =.ores= files tangled from literate
 * org sources — pristine build artefacts. /My Scripts/ are the
 * operator's own =.ores= files in a writable per-user directory.
 * Activating a script (double-click or Enter) emits openRequested() so
 * the embedding window can open it in a standalone script editor; the
 * panel itself neither edits nor runs anything. Whether a script is a
 * read-only library template is carried in the signal, not shown as
 * text — the editor expresses it by disabling Save.
 */
class ScriptLibraryPanel : public QWidget {
    Q_OBJECT

private:
    inline static std::string_view logger_name = "ores.qt.script_library_panel";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    explicit ScriptLibraryPanel(QWidget* parent = nullptr);

    /**
     * @brief Re-scan both script directories and rebuild the tree.
     */
    void refresh();

    /**
     * @brief Absolute path of the shipped library scripts directory.
     *
     * Resolved, in order: the =ORES_SHELL_SCRIPTS_DIR= environment
     * variable; =<app-dir>/../share/ores/shell-scripts=; and the
     * development repo path =projects/ores.shell/scripts=.
     */
    static QString library_dir();

    /**
     * @brief Absolute path of the writable per-user scripts directory
     * (created on first use): =<AppDataLocation>/scripts=.
     */
    static QString user_dir();

signals:
    /**
     * @brief The user activated the script at @p path; @p library is
     * true for a read-only library template. The embedding window opens
     * it in a script editor.
     */
    void openRequested(const QString& path, bool library);

    /// Status-bar text for the embedding window.
    void statusChanged(const QString& message);

private slots:
    void on_filter_changed(const QString& text);
    void on_item_activated(QTreeWidgetItem* item, int column);

private:
    void setup_ui();
    void add_scripts(QTreeWidgetItem* group, const QString& dir, bool library);

    QLineEdit* filter_{nullptr};
    QTreeWidget* tree_{nullptr};
};

}

#endif
