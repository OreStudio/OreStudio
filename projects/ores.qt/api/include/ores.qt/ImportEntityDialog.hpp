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
#ifndef ORES_QT_IMPORT_ENTITY_DIALOG_HPP
#define ORES_QT_IMPORT_ENTITY_DIALOG_HPP

#include "ores.logging/make_logger.hpp"
#include "ores.qt/export.hpp"
#include <QCheckBox>
#include <QDialog>
#include <QLabel>
#include <QProgressBar>
#include <QPushButton>
#include <QString>
#include <QStringList>
#include <QTableWidget>
#include <atomic>
#include <functional>
#include <vector>

namespace ores::qt {

/**
 * @brief One row of preview data in an ImportEntityDialog.
 *
 * @c display_values must have the same length as the dialog's column
 * headers. Rows with @c is_valid false are shown unchecked, disabled,
 * and highlighted, with @c invalid_reason as the tooltip.
 */
struct ImportEntityRow {
    QStringList display_values;
    bool is_valid = true;
    QString invalid_reason;
};

/**
 * @brief Generic preview-and-import dialog for entities parsed from a
 * file (XML, CSV, ...).
 *
 * Qt's moc cannot process class templates with Q_OBJECT, so this is a
 * single concrete class driven by type-erased row data and callbacks,
 * rather than a template parametrized on the entity type — callers
 * (e.g. codegen'd *MdiWindow::importFromXML()) build the preview rows
 * and supply an @c import_one callback closing over the parsed entity
 * vector, client manager, and username.
 *
 * Mirrors the currency-specific dialog this generalises: checkboxes
 * for selecting which rows to import, a preview table, a progress bar
 * and cancel button during import.
 */
class ORES_QT_API ImportEntityDialog final : public QDialog {
    Q_OBJECT

private:
    inline static std::string_view logger_name = "ores.qt.import_entity_dialog";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    /**
     * @param entity_name_plural Used in the window title, e.g. "Currencies".
     * @param filename Path to the file being imported, shown to the user.
     * @param column_headers Table column headers (excluding the checkbox column).
     * @param rows Preview rows, one per parsed entity, in the same
     * order @c import_one indexes into.
     * @param label_of Returns the short label (e.g. an ISO code) shown
     * in the progress status text for row @p index.
     * @param import_one Imports the entity at @p index. Called on a
     * background thread; must be safe to call from there. Returns
     * true on success.
     */
    explicit ImportEntityDialog(const QString& entity_name_plural,
                                const QString& filename,
                                const QStringList& column_headers,
                                std::vector<ImportEntityRow> rows,
                                std::function<QString(std::size_t index)> label_of,
                                std::function<bool(std::size_t index)> import_one,
                                QWidget* parent = nullptr);

    ~ImportEntityDialog() override;

    /**
     * @brief Row indices selected for import (checkbox checked).
     */
    [[nodiscard]] std::vector<std::size_t> getSelectedIndices() const;

signals:
    /**
     * @brief Emitted when import completes (successfully or partially).
     * @param success_count Number of entities successfully imported.
     * @param total_count Total number of entities attempted.
     */
    void importCompleted(int success_count, int total_count);

    /**
     * @brief Emitted when import is cancelled.
     */
    void importCancelled();

private slots:
    void onImportClicked();
    void onCancelClicked();
    void onSelectAllChanged(int state);
    void onRowCheckChanged();

private:
    void setupUI(const QString& entity_name_plural,
                const QString& filename,
                const QStringList& column_headers);
    void populateTable();
    void updateSelectionCount();
    void updateImportButtonState();

private:
    std::vector<ImportEntityRow> rows_;
    std::function<QString(std::size_t)> label_of_;
    std::function<bool(std::size_t)> import_one_;

    QLabel* filenameLabel_;
    QCheckBox* selectAllCheckbox_;
    QLabel* selectionCountLabel_;
    QTableWidget* table_;
    QProgressBar* progressBar_;
    QLabel* statusLabel_;
    QPushButton* importButton_;
    QPushButton* cancelButton_;

    bool importInProgress_;
    std::atomic<bool> cancelRequested_;
};

}

#endif
