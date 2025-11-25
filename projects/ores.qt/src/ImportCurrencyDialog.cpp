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
#include "ores.qt/ImportCurrencyDialog.hpp"

#include <QVBoxLayout>
#include <QHBoxLayout>
#include <QHeaderView>
#include <QFileInfo>
#include <QtConcurrent/QtConcurrent>
#include "ores.risk/messaging/protocol.hpp"
#include "ores.comms/protocol/frame.hpp"

namespace ores::qt {

using namespace ores::utility::log;

ImportCurrencyDialog::ImportCurrencyDialog(
    const std::vector<risk::domain::currency>& currencies,
    const QString& filename,
    std::shared_ptr<comms::net::client> client,
    QWidget* parent)
    : QDialog(parent),
      currencies_(currencies),
      filename_(filename),
      client_(std::move(client)),
      importInProgress_(false) {

    BOOST_LOG_SEV(lg(), debug) << "Creating import currency dialog for file: "
                               << filename.toStdString()
                               << " with " << currencies.size() << " currencies";

    setupUI();
    populateTable();
    updateSelectionCount();
}

ImportCurrencyDialog::~ImportCurrencyDialog() {
    BOOST_LOG_SEV(lg(), debug) << "Destroying import currency dialog";
}

void ImportCurrencyDialog::setupUI() {
    setWindowTitle("Import Currencies from XML");
    setModal(true);
    resize(800, 600);

    auto* mainLayout = new QVBoxLayout(this);

    // Filename label
    QFileInfo fileInfo(filename_);
    filenameLabel_ = new QLabel(QString("File: %1").arg(fileInfo.fileName()));
    mainLayout->addWidget(filenameLabel_);

    // Select All checkbox and count label
    auto* selectionLayout = new QHBoxLayout();
    selectAllCheckbox_ = new QCheckBox("Select All");
    selectAllCheckbox_->setChecked(true);
    connect(selectAllCheckbox_, &QCheckBox::checkStateChanged,
            this, &ImportCurrencyDialog::onSelectAllChanged);
    selectionLayout->addWidget(selectAllCheckbox_);

    selectionCountLabel_ = new QLabel();
    selectionLayout->addWidget(selectionCountLabel_);
    selectionLayout->addStretch();
    mainLayout->addLayout(selectionLayout);

    // Currency table
    currencyTable_ = new QTableWidget(this);
    currencyTable_->setColumnCount(6);
    currencyTable_->setHorizontalHeaderLabels({
        "", "ISO Code", "Name", "Symbol", "Fraction Symbol", "Fractions/Unit"
    });
    currencyTable_->setSelectionBehavior(QAbstractItemView::SelectRows);
    currencyTable_->setEditTriggers(QAbstractItemView::NoEditTriggers);
    currencyTable_->horizontalHeader()->setStretchLastSection(false);
    currencyTable_->horizontalHeader()->setSectionResizeMode(
        QHeaderView::ResizeToContents);
    currencyTable_->verticalHeader()->setVisible(false);
    mainLayout->addWidget(currencyTable_);

    // Progress bar
    progressBar_ = new QProgressBar(this);
    progressBar_->setVisible(false);
    progressBar_->setTextVisible(true);
    mainLayout->addWidget(progressBar_);

    // Status label
    statusLabel_ = new QLabel();
    statusLabel_->setVisible(false);
    mainLayout->addWidget(statusLabel_);

    // Buttons
    auto* buttonLayout = new QHBoxLayout();
    buttonLayout->addStretch();

    importButton_ = new QPushButton("Import");
    connect(importButton_, &QPushButton::clicked,
            this, &ImportCurrencyDialog::onImportClicked);
    buttonLayout->addWidget(importButton_);

    cancelButton_ = new QPushButton("Cancel");
    connect(cancelButton_, &QPushButton::clicked,
            this, &ImportCurrencyDialog::onCancelClicked);
    buttonLayout->addWidget(cancelButton_);

    mainLayout->addLayout(buttonLayout);
}

void ImportCurrencyDialog::populateTable() {
    BOOST_LOG_SEV(lg(), debug) << "Populating table with "
                               << currencies_.size() << " currencies";

    currencyTable_->setRowCount(static_cast<int>(currencies_.size()));

    for (size_t i = 0; i < currencies_.size(); ++i) {
        const auto& currency = currencies_[i];

        // Checkbox column
        auto* checkBoxWidget = new QWidget();
        auto* checkBoxLayout = new QHBoxLayout(checkBoxWidget);
        checkBoxLayout->setContentsMargins(0, 0, 0, 0);
        checkBoxLayout->setAlignment(Qt::AlignCenter);

        auto* checkBox = new QCheckBox();
        checkBox->setChecked(true);
        checkBox->setProperty("row", static_cast<int>(i));
        connect(checkBox, &QCheckBox::checkStateChanged,
                this, &ImportCurrencyDialog::onCurrencyCheckChanged);
        checkBoxLayout->addWidget(checkBox);

        currencyTable_->setCellWidget(static_cast<int>(i), 0, checkBoxWidget);

        // Currency data columns
        currencyTable_->setItem(static_cast<int>(i), 1,
            new QTableWidgetItem(QString::fromStdString(currency.iso_code)));
        currencyTable_->setItem(static_cast<int>(i), 2,
            new QTableWidgetItem(QString::fromStdString(currency.name)));
        currencyTable_->setItem(static_cast<int>(i), 3,
            new QTableWidgetItem(QString::fromStdString(currency.symbol)));
        currencyTable_->setItem(static_cast<int>(i), 4,
            new QTableWidgetItem(QString::fromStdString(currency.fraction_symbol)));
        currencyTable_->setItem(static_cast<int>(i), 5,
            new QTableWidgetItem(QString::number(currency.fractions_per_unit)));
    }

    BOOST_LOG_SEV(lg(), debug) << "Table populated successfully";
}

void ImportCurrencyDialog::updateSelectionCount() {
    int selectedCount = 0;
    for (int i = 0; i < currencyTable_->rowCount(); ++i) {
        auto* cellWidget = currencyTable_->cellWidget(i, 0);
        if (cellWidget) {
            auto* checkBox = cellWidget->findChild<QCheckBox*>();
            if (checkBox && checkBox->isChecked()) {
                selectedCount++;
            }
        }
    }

    selectionCountLabel_->setText(
        QString("(%1 of %2 currencies selected)")
        .arg(selectedCount)
        .arg(currencies_.size()));

    updateImportButtonState();
}

void ImportCurrencyDialog::updateImportButtonState() {
    int selectedCount = 0;
    for (int i = 0; i < currencyTable_->rowCount(); ++i) {
        auto* cellWidget = currencyTable_->cellWidget(i, 0);
        if (cellWidget) {
            auto* checkBox = cellWidget->findChild<QCheckBox*>();
            if (checkBox && checkBox->isChecked()) {
                selectedCount++;
            }
        }
    }

    importButton_->setEnabled(selectedCount > 0 && !importInProgress_);
}

std::vector<risk::domain::currency>
ImportCurrencyDialog::getSelectedCurrencies() const {
    std::vector<risk::domain::currency> selected;

    for (int i = 0; i < currencyTable_->rowCount(); ++i) {
        auto* cellWidget = currencyTable_->cellWidget(i, 0);
        if (cellWidget) {
            auto* checkBox = cellWidget->findChild<QCheckBox*>();
            if (checkBox && checkBox->isChecked()) {
                selected.push_back(currencies_[static_cast<size_t>(i)]);
            }
        }
    }

    BOOST_LOG_SEV(lg(), debug) << "Retrieved " << selected.size()
                               << " selected currencies";
    return selected;
}

void ImportCurrencyDialog::onSelectAllChanged(int state) {
    BOOST_LOG_SEV(lg(), debug) << "Select all changed: " << state;

    const bool checked = (state == Qt::Checked);

    for (int i = 0; i < currencyTable_->rowCount(); ++i) {
        auto* cellWidget = currencyTable_->cellWidget(i, 0);
        if (cellWidget) {
            auto* checkBox = cellWidget->findChild<QCheckBox*>();
            if (checkBox) {
                checkBox->setChecked(checked);
            }
        }
    }

    updateSelectionCount();
}

void ImportCurrencyDialog::onCurrencyCheckChanged() {
    updateSelectionCount();

    // Update Select All checkbox state
    int checkedCount = 0;
    for (int i = 0; i < currencyTable_->rowCount(); ++i) {
        auto* cellWidget = currencyTable_->cellWidget(i, 0);
        if (cellWidget) {
            auto* checkBox = cellWidget->findChild<QCheckBox*>();
            if (checkBox && checkBox->isChecked()) {
                checkedCount++;
            }
        }
    }

    selectAllCheckbox_->blockSignals(true);
    if (checkedCount == 0) {
        selectAllCheckbox_->setCheckState(Qt::Unchecked);
    } else if (checkedCount == currencyTable_->rowCount()) {
        selectAllCheckbox_->setCheckState(Qt::Checked);
    } else {
        selectAllCheckbox_->setCheckState(Qt::PartiallyChecked);
    }
    selectAllCheckbox_->blockSignals(false);
}

void ImportCurrencyDialog::onImportClicked() {
    BOOST_LOG_SEV(lg(), debug) << "Import button clicked";

    importInProgress_ = true;

    // Disable UI during import
    importButton_->setEnabled(false);
    selectAllCheckbox_->setEnabled(false);
    currencyTable_->setEnabled(false);

    // Show progress bar
    progressBar_->setVisible(true);
    statusLabel_->setVisible(true);

    // Get selected currencies
    const auto selected = getSelectedCurrencies();
    const int total = static_cast<int>(selected.size());

    BOOST_LOG_SEV(lg(), info) << "Starting import of " << total << " currencies";

    progressBar_->setRange(0, total);
    progressBar_->setValue(0);
    statusLabel_->setText("Starting import...");

    // Create a shared pointer to track progress safely
    auto self = this;

    // Run import in background thread
    QFuture<std::pair<int, int>> future =
        QtConcurrent::run([self, selected, total]() -> std::pair<int, int> {
            using namespace ores::risk::messaging;

            int success_count = 0;
            int current = 0;

            for (const auto& currency : selected) {
                current++;

                // Update UI on main thread
                QMetaObject::invokeMethod(self, [self, currency, current, total]() {
                    self->progressBar_->setValue(current);
                    self->statusLabel_->setText(
                        QString("Importing %1 (%2 of %3)...")
                        .arg(QString::fromStdString(currency.iso_code))
                        .arg(current)
                        .arg(total));
                }, Qt::QueuedConnection);

                try {
                    save_currency_request request{currency};
                    auto payload = request.serialize();
                    comms::protocol::frame request_frame(
                        comms::protocol::message_type::save_currency_request,
                        0, std::move(payload));

                    auto response_result = self->client_->send_request_sync(
                        std::move(request_frame));

                    if (!response_result) {
                        BOOST_LOG_SEV(lg(), warn)
                            << "Failed to import currency: "
                            << currency.iso_code;
                        continue;
                    }

                    auto response = save_currency_response::
                        deserialize(response_result->payload());

                    if (response && response->success) {
                        success_count++;
                        BOOST_LOG_SEV(lg(), debug)
                            << "Successfully imported: " << currency.iso_code;
                    } else {
                        BOOST_LOG_SEV(lg(), warn)
                            << "Server rejected currency: "
                            << currency.iso_code;
                    }
                } catch (const std::exception& e) {
                    BOOST_LOG_SEV(lg(), error)
                        << "Error importing currency "
                        << currency.iso_code << ": " << e.what();
                }
            }

            return {success_count, total};
        });

    // Watch for completion
    auto* watcher = new QFutureWatcher<std::pair<int, int>>(this);
    connect(watcher, &QFutureWatcher<std::pair<int, int>>::finished,
            this, [this, watcher]() {
        auto result = watcher->result();
        const int success_count = result.first;
        const int total_count = result.second;

        BOOST_LOG_SEV(lg(), info)
            << "Import completed: " << success_count
            << " of " << total_count << " currencies imported successfully";

        // Hide progress bar
        progressBar_->setVisible(false);
        statusLabel_->setVisible(false);

        importInProgress_ = false;

        // Emit completion signal
        emit importCompleted(success_count, total_count);

        // Close dialog
        accept();

        watcher->deleteLater();
    });

    watcher->setFuture(future);
}

void ImportCurrencyDialog::onCancelClicked() {
    BOOST_LOG_SEV(lg(), debug) << "Cancel button clicked";

    if (importInProgress_) {
        // TODO: Phase 2 - Cancel ongoing import
        emit importCancelled();
    }

    reject();
}

}
