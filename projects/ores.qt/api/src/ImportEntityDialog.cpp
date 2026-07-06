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
#include "ores.qt/ImportEntityDialog.hpp"
#include "ores.qt/MessageBoxHelper.hpp"
#include <QFileInfo>
#include <QFutureWatcher>
#include <QHBoxLayout>
#include <QHeaderView>
#include <QVBoxLayout>
#include <QtConcurrent/QtConcurrent>

namespace ores::qt {

using namespace ores::logging;

ImportEntityDialog::ImportEntityDialog(const QString& entity_name_plural,
                                       const QString& filename,
                                       const QStringList& column_headers,
                                       std::vector<ImportEntityRow> rows,
                                       std::function<QString(std::size_t)> label_of,
                                       std::function<bool(std::size_t)> import_one,
                                       QWidget* parent)
    : QDialog(parent)
    , rows_(std::move(rows))
    , label_of_(std::move(label_of))
    , import_one_(std::move(import_one))
    , importInProgress_(false)
    , cancelRequested_(false) {

    BOOST_LOG_SEV(lg(), debug) << "Creating import entity dialog for file: "
                               << filename.toStdString() << " with " << rows_.size() << " rows";

    setupUI(entity_name_plural, filename, column_headers);
    populateTable();
    updateSelectionCount();
}

ImportEntityDialog::~ImportEntityDialog() {
    BOOST_LOG_SEV(lg(), debug) << "Destroying import entity dialog";
}

void ImportEntityDialog::setupUI(const QString& entity_name_plural,
                                 const QString& filename,
                                 const QStringList& column_headers) {
    setWindowTitle(QString("Import %1 from XML").arg(entity_name_plural));
    setModal(true);
    resize(800, 600);

    auto* mainLayout = new QVBoxLayout(this);

    QFileInfo fileInfo(filename);
    filenameLabel_ = new QLabel(QString("File: %1").arg(fileInfo.fileName()));
    mainLayout->addWidget(filenameLabel_);

    auto* selectionLayout = new QHBoxLayout();
    selectAllCheckbox_ = new QCheckBox("Select All");
    selectAllCheckbox_->setChecked(true);
    // Using stateChanged for Qt 6.x compatibility (checkStateChanged added in 6.7)
    QT_WARNING_PUSH
    QT_WARNING_DISABLE_DEPRECATED
    connect(
        selectAllCheckbox_, &QCheckBox::stateChanged, this, &ImportEntityDialog::onSelectAllChanged);
    QT_WARNING_POP
    selectionLayout->addWidget(selectAllCheckbox_);

    selectionCountLabel_ = new QLabel();
    selectionLayout->addWidget(selectionCountLabel_);
    selectionLayout->addStretch();
    mainLayout->addLayout(selectionLayout);

    table_ = new QTableWidget(this);
    table_->setColumnCount(column_headers.size() + 1);
    QStringList headers;
    headers << "" << column_headers;
    table_->setHorizontalHeaderLabels(headers);
    table_->setSelectionBehavior(QAbstractItemView::SelectRows);
    table_->setEditTriggers(QAbstractItemView::NoEditTriggers);
    table_->horizontalHeader()->setStretchLastSection(false);
    table_->horizontalHeader()->setSectionResizeMode(QHeaderView::ResizeToContents);
    table_->verticalHeader()->setVisible(false);
    mainLayout->addWidget(table_);

    progressBar_ = new QProgressBar(this);
    progressBar_->setVisible(false);
    progressBar_->setTextVisible(true);
    mainLayout->addWidget(progressBar_);

    statusLabel_ = new QLabel();
    statusLabel_->setVisible(false);
    mainLayout->addWidget(statusLabel_);

    auto* buttonLayout = new QHBoxLayout();
    buttonLayout->addStretch();

    importButton_ = new QPushButton("Import");
    connect(importButton_, &QPushButton::clicked, this, &ImportEntityDialog::onImportClicked);
    buttonLayout->addWidget(importButton_);

    cancelButton_ = new QPushButton("Cancel");
    connect(cancelButton_, &QPushButton::clicked, this, &ImportEntityDialog::onCancelClicked);
    buttonLayout->addWidget(cancelButton_);

    mainLayout->addLayout(buttonLayout);
}

void ImportEntityDialog::populateTable() {
    BOOST_LOG_SEV(lg(), debug) << "Populating table with " << rows_.size() << " rows";

    table_->setRowCount(static_cast<int>(rows_.size()));

    int valid_count = 0;
    int invalid_count = 0;

    for (std::size_t i = 0; i < rows_.size(); ++i) {
        const auto& row = rows_[i];

        auto* checkBoxWidget = new QWidget();
        auto* checkBoxLayout = new QHBoxLayout(checkBoxWidget);
        checkBoxLayout->setContentsMargins(0, 0, 0, 0);
        checkBoxLayout->setAlignment(Qt::AlignCenter);

        auto* checkBox = new QCheckBox();
        checkBox->setProperty("row", static_cast<int>(i));
        QT_WARNING_PUSH
        QT_WARNING_DISABLE_DEPRECATED
        connect(checkBox, &QCheckBox::stateChanged, this, &ImportEntityDialog::onRowCheckChanged);
        QT_WARNING_POP

        if (!row.is_valid) {
            checkBox->setChecked(false);
            checkBox->setEnabled(false);
            checkBoxWidget->setToolTip(QString("Cannot import: %1").arg(row.invalid_reason));
            invalid_count++;
        } else {
            checkBox->setChecked(true);
            valid_count++;
        }

        checkBoxLayout->addWidget(checkBox);
        table_->setCellWidget(static_cast<int>(i), 0, checkBoxWidget);

        const QBrush errorBrush(QColor(255, 200, 200));
        const QString tooltip = QString("Validation errors:\n%1").arg(row.invalid_reason);

        for (int col = 0; col < row.display_values.size(); ++col) {
            auto* item = new QTableWidgetItem(row.display_values[col]);
            if (!row.is_valid) {
                item->setBackground(errorBrush);
                item->setToolTip(tooltip);
            }
            table_->setItem(static_cast<int>(i), col + 1, item);
        }
    }

    BOOST_LOG_SEV(lg(), debug) << "Table populated successfully - " << valid_count << " valid, "
                               << invalid_count << " invalid rows";
}

void ImportEntityDialog::updateSelectionCount() {
    int selectedCount = 0;
    for (int i = 0; i < table_->rowCount(); ++i) {
        auto* cellWidget = table_->cellWidget(i, 0);
        if (cellWidget) {
            auto* checkBox = cellWidget->findChild<QCheckBox*>();
            if (checkBox && checkBox->isChecked())
                selectedCount++;
        }
    }

    selectionCountLabel_->setText(
        QString("(%1 of %2 selected)").arg(selectedCount).arg(rows_.size()));

    updateImportButtonState();
}

void ImportEntityDialog::updateImportButtonState() {
    int selectedCount = 0;
    for (int i = 0; i < table_->rowCount(); ++i) {
        auto* cellWidget = table_->cellWidget(i, 0);
        if (cellWidget) {
            auto* checkBox = cellWidget->findChild<QCheckBox*>();
            if (checkBox && checkBox->isChecked())
                selectedCount++;
        }
    }

    importButton_->setEnabled(selectedCount > 0 && !importInProgress_);
}

std::vector<std::size_t> ImportEntityDialog::getSelectedIndices() const {
    std::vector<std::size_t> selected;

    for (int i = 0; i < table_->rowCount(); ++i) {
        auto* cellWidget = table_->cellWidget(i, 0);
        if (cellWidget) {
            auto* checkBox = cellWidget->findChild<QCheckBox*>();
            if (checkBox && checkBox->isChecked())
                selected.push_back(static_cast<std::size_t>(i));
        }
    }

    BOOST_LOG_SEV(lg(), debug) << "Retrieved " << selected.size() << " selected rows";
    return selected;
}

void ImportEntityDialog::onSelectAllChanged(int state) {
    const bool checked = (state == Qt::Checked);

    for (int i = 0; i < table_->rowCount(); ++i) {
        auto* cellWidget = table_->cellWidget(i, 0);
        if (cellWidget) {
            auto* checkBox = cellWidget->findChild<QCheckBox*>();
            if (checkBox)
                checkBox->setChecked(checked);
        }
    }

    updateSelectionCount();
}

void ImportEntityDialog::onRowCheckChanged() {
    updateSelectionCount();

    int checkedCount = 0;
    for (int i = 0; i < table_->rowCount(); ++i) {
        auto* cellWidget = table_->cellWidget(i, 0);
        if (cellWidget) {
            auto* checkBox = cellWidget->findChild<QCheckBox*>();
            if (checkBox && checkBox->isChecked())
                checkedCount++;
        }
    }

    selectAllCheckbox_->blockSignals(true);
    if (checkedCount == 0) {
        selectAllCheckbox_->setCheckState(Qt::Unchecked);
    } else if (checkedCount == table_->rowCount()) {
        selectAllCheckbox_->setCheckState(Qt::Checked);
    } else {
        selectAllCheckbox_->setCheckState(Qt::PartiallyChecked);
    }
    selectAllCheckbox_->blockSignals(false);
}

void ImportEntityDialog::onImportClicked() {
    BOOST_LOG_SEV(lg(), debug) << "Import button clicked";

    importInProgress_ = true;

    importButton_->setEnabled(false);
    selectAllCheckbox_->setEnabled(false);
    table_->setEnabled(false);

    progressBar_->setVisible(true);
    statusLabel_->setVisible(true);

    const auto selected = getSelectedIndices();
    const int total = static_cast<int>(selected.size());

    BOOST_LOG_SEV(lg(), info) << "Starting import of " << total << " rows";

    progressBar_->setRange(0, total);
    progressBar_->setValue(0);
    statusLabel_->setText("Starting import...");

    auto self = this;
    auto import_one = import_one_;
    auto label_of = label_of_;

    QFuture<std::pair<int, int>> future =
        QtConcurrent::run([self, selected, total, import_one, label_of]() -> std::pair<int, int> {
            int success_count = 0;
            int current = 0;

            for (auto index : selected) {
                if (self->cancelRequested_.load()) {
                    BOOST_LOG_SEV(lg(), info)
                        << "Import cancelled by user at row " << current << " of " << total;
                    break;
                }

                current++;
                const auto label = label_of(index);

                QMetaObject::invokeMethod(
                    self,
                    [self, label, current, total]() {
                        self->progressBar_->setValue(current);
                        self->statusLabel_->setText(
                            QString("Importing %1 (%2 of %3)...").arg(label).arg(current).arg(total));
                    },
                    Qt::QueuedConnection);

                if (import_one(index))
                    success_count++;
            }

            return {success_count, total};
        });

    auto* watcher = new QFutureWatcher<std::pair<int, int>>(this);
    connect(watcher, &QFutureWatcher<std::pair<int, int>>::finished, this, [this, watcher]() {
        auto result = watcher->result();
        const int success_count = result.first;
        const int total_count = result.second;

        progressBar_->setVisible(false);
        statusLabel_->setVisible(false);

        importInProgress_ = false;

        if (cancelRequested_.load()) {
            BOOST_LOG_SEV(lg(), info)
                << "Import cancelled after importing " << success_count << " of " << total_count;
            emit importCancelled();
            reject();
        } else {
            BOOST_LOG_SEV(lg(), info)
                << "Import completed: " << success_count << " of " << total_count << " imported";
            emit importCompleted(success_count, total_count);
            accept();
        }

        watcher->deleteLater();
    });

    watcher->setFuture(future);
}

void ImportEntityDialog::onCancelClicked() {
    BOOST_LOG_SEV(lg(), debug) << "Cancel button clicked";

    if (importInProgress_) {
        cancelRequested_.store(true);
        BOOST_LOG_SEV(lg(), info) << "Cancellation requested";

        statusLabel_->setText("Cancelling import...");
        cancelButton_->setEnabled(false);
    } else {
        reject();
    }
}

}
