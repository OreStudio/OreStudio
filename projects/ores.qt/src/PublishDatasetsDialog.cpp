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
#include "ores.qt/PublishDatasetsDialog.hpp"

#include <QVBoxLayout>
#include <QHBoxLayout>
#include <QFormLayout>
#include <QGroupBox>
#include <QHeaderView>
#include <QMessageBox>
#include <boost/uuid/uuid_io.hpp>
#include "ores.qt/ClientManager.hpp"
#include "ores.dq/messaging/publication_protocol.hpp"

namespace ores::qt {

using namespace ores::logging;

PublishDatasetsDialog::PublishDatasetsDialog(
    ClientManager* clientManager,
    const QString& username,
    QWidget* parent)
    : QDialog(parent),
      clientManager_(clientManager),
      username_(username) {

    setWindowTitle(tr("Publish Datasets"));
    setMinimumSize(600, 500);
    resize(700, 550);

    setupUi();
    setupConnections();
}

void PublishDatasetsDialog::setupUi() {
    auto* mainLayout = new QVBoxLayout(this);
    mainLayout->setSpacing(16);

    // Datasets group
    auto* datasetsGroup = new QGroupBox(tr("Datasets to Publish"), this);
    auto* datasetsLayout = new QVBoxLayout(datasetsGroup);

    countLabel_ = new QLabel(this);
    datasetsLayout->addWidget(countLabel_);

    datasetList_ = new QListWidget(this);
    datasetList_->setMaximumHeight(120);
    datasetList_->setSelectionMode(QAbstractItemView::NoSelection);
    datasetsLayout->addWidget(datasetList_);

    mainLayout->addWidget(datasetsGroup);

    // Options group
    auto* optionsGroup = new QGroupBox(tr("Publication Options"), this);
    auto* optionsLayout = new QFormLayout(optionsGroup);

    modeCombo_ = new QComboBox(this);
    modeCombo_->addItem(tr("Upsert (insert new, update existing)"), "upsert");
    modeCombo_->addItem(tr("Insert Only (skip existing)"), "insert_only");
    modeCombo_->addItem(tr("Replace All (delete then insert)"), "replace_all");
    optionsLayout->addRow(tr("Mode:"), modeCombo_);

    resolveDependenciesCheck_ = new QCheckBox(tr("Automatically publish dependencies first"), this);
    resolveDependenciesCheck_->setChecked(true);
    optionsLayout->addRow(tr("Dependencies:"), resolveDependenciesCheck_);

    mainLayout->addWidget(optionsGroup);

    // Progress bar (hidden initially)
    progressBar_ = new QProgressBar(this);
    progressBar_->setRange(0, 0);  // Indeterminate
    progressBar_->setVisible(false);
    mainLayout->addWidget(progressBar_);

    // Results group (hidden initially)
    auto* resultsGroup = new QGroupBox(tr("Results"), this);
    auto* resultsLayout = new QVBoxLayout(resultsGroup);

    summaryLabel_ = new QLabel(this);
    resultsLayout->addWidget(summaryLabel_);

    resultsTable_ = new QTableWidget(this);
    resultsTable_->setColumnCount(6);
    resultsTable_->setHorizontalHeaderLabels({
        tr("Dataset"),
        tr("Target Table"),
        tr("Inserted"),
        tr("Skipped"),
        tr("Deleted"),
        tr("Status")
    });
    resultsTable_->horizontalHeader()->setStretchLastSection(true);
    resultsTable_->horizontalHeader()->setSectionResizeMode(0, QHeaderView::Stretch);
    resultsTable_->setSelectionBehavior(QAbstractItemView::SelectRows);
    resultsTable_->setEditTriggers(QAbstractItemView::NoEditTriggers);
    resultsTable_->setAlternatingRowColors(true);
    resultsLayout->addWidget(resultsTable_);

    resultsGroup->setVisible(false);
    mainLayout->addWidget(resultsGroup, 1);

    // Buttons
    auto* buttonLayout = new QHBoxLayout();
    buttonLayout->addStretch();

    publishButton_ = new QPushButton(tr("Publish"), this);
    publishButton_->setDefault(true);
    buttonLayout->addWidget(publishButton_);

    closeButton_ = new QPushButton(tr("Close"), this);
    buttonLayout->addWidget(closeButton_);

    mainLayout->addLayout(buttonLayout);
}

void PublishDatasetsDialog::setupConnections() {
    connect(publishButton_, &QPushButton::clicked,
            this, &PublishDatasetsDialog::onPublishClicked);
    connect(closeButton_, &QPushButton::clicked,
            this, &PublishDatasetsDialog::onCloseClicked);
}

void PublishDatasetsDialog::setDatasets(
    const std::vector<dq::domain::dataset>& datasets) {

    datasets_ = datasets;
    datasetList_->clear();

    for (const auto& ds : datasets_) {
        auto item = new QListWidgetItem(
            QString::fromStdString(ds.code) + " - " +
            QString::fromStdString(ds.name));
        datasetList_->addItem(item);
    }

    countLabel_->setText(tr("%1 dataset(s) selected")
        .arg(datasets_.size()));

    publishButton_->setEnabled(!datasets_.empty());
}

void PublishDatasetsDialog::onPublishClicked() {
    BOOST_LOG_SEV(lg(), info) << "Publishing " << datasets_.size() << " datasets";

    if (datasets_.empty()) {
        QMessageBox::warning(this, tr("No Datasets"),
            tr("No datasets selected for publication."));
        return;
    }

    // Get mode
    auto modeStr = modeCombo_->currentData().toString().toStdString();
    auto mode = dq::domain::publication_mode_from_string(modeStr);
    if (!mode) {
        BOOST_LOG_SEV(lg(), error) << "Invalid publication mode: " << modeStr;
        QMessageBox::critical(this, tr("Error"),
            tr("Invalid publication mode selected."));
        return;
    }

    // Build request
    dq::messaging::publish_datasets_request request;
    for (const auto& ds : datasets_) {
        request.dataset_ids.push_back(ds.id);
    }
    request.mode = *mode;
    request.published_by = username_.toStdString();
    request.resolve_dependencies = resolveDependenciesCheck_->isChecked();

    BOOST_LOG_SEV(lg(), info) << "Sending publish request: "
        << "mode=" << request.mode
        << ", resolve_dependencies=" << request.resolve_dependencies
        << ", datasets=" << request.dataset_ids.size();

    // Show progress
    setUiEnabled(false);
    progressBar_->setVisible(true);

    // Send request
    auto result = clientManager_->process_request(std::move(request));

    progressBar_->setVisible(false);
    setUiEnabled(true);

    if (!result) {
        BOOST_LOG_SEV(lg(), error) << "Publication request failed";
        QMessageBox::critical(this, tr("Publication Failed"),
            tr("Failed to publish datasets. Please check the server logs."));
        return;
    }

    BOOST_LOG_SEV(lg(), info) << "Publication complete: "
        << result->results.size() << " results";

    showResults(result->results);
}

void PublishDatasetsDialog::showResults(
    const std::vector<dq::domain::publication_result>& results) {

    // Show results group
    auto* resultsGroup = findChild<QGroupBox*>();
    if (resultsGroup && resultsGroup->title() == tr("Results")) {
        resultsGroup->setVisible(true);
    }

    // Populate table
    resultsTable_->setRowCount(static_cast<int>(results.size()));

    std::uint64_t totalInserted = 0;
    std::uint64_t totalSkipped = 0;
    std::uint64_t totalDeleted = 0;
    int successCount = 0;
    int failureCount = 0;

    for (int row = 0; row < static_cast<int>(results.size()); ++row) {
        const auto& r = results[row];

        auto* codeItem = new QTableWidgetItem(
            QString::fromStdString(r.dataset_code));
        resultsTable_->setItem(row, 0, codeItem);

        auto* tableItem = new QTableWidgetItem(
            QString::fromStdString(r.target_table));
        resultsTable_->setItem(row, 1, tableItem);

        auto* insertedItem = new QTableWidgetItem(
            QString::number(r.records_inserted));
        insertedItem->setTextAlignment(Qt::AlignRight | Qt::AlignVCenter);
        resultsTable_->setItem(row, 2, insertedItem);

        auto* skippedItem = new QTableWidgetItem(
            QString::number(r.records_skipped));
        skippedItem->setTextAlignment(Qt::AlignRight | Qt::AlignVCenter);
        resultsTable_->setItem(row, 3, skippedItem);

        auto* deletedItem = new QTableWidgetItem(
            QString::number(r.records_deleted));
        deletedItem->setTextAlignment(Qt::AlignRight | Qt::AlignVCenter);
        resultsTable_->setItem(row, 4, deletedItem);

        QString statusText = r.success ? tr("Success") : tr("Failed");
        auto* statusItem = new QTableWidgetItem(statusText);
        if (!r.success) {
            statusItem->setForeground(Qt::red);
            statusItem->setToolTip(QString::fromStdString(r.error_message));
        } else {
            statusItem->setForeground(Qt::darkGreen);
        }
        resultsTable_->setItem(row, 5, statusItem);

        // Accumulate totals
        if (r.success) {
            ++successCount;
            totalInserted += r.records_inserted;
            totalSkipped += r.records_skipped;
            totalDeleted += r.records_deleted;
        } else {
            ++failureCount;
        }
    }

    // Update summary
    QString summary = tr("Published %1 dataset(s): %2 succeeded, %3 failed. "
                         "Total: %4 inserted, %5 skipped, %6 deleted.")
        .arg(results.size())
        .arg(successCount)
        .arg(failureCount)
        .arg(totalInserted)
        .arg(totalSkipped)
        .arg(totalDeleted);
    summaryLabel_->setText(summary);

    BOOST_LOG_SEV(lg(), info) << "Publication results: "
        << successCount << " succeeded, "
        << failureCount << " failed, "
        << totalInserted << " inserted, "
        << totalSkipped << " skipped, "
        << totalDeleted << " deleted";

    // Change publish button to "Done"
    publishButton_->setText(tr("Publish Again"));
}

void PublishDatasetsDialog::onCloseClicked() {
    accept();
}

void PublishDatasetsDialog::setUiEnabled(bool enabled) {
    modeCombo_->setEnabled(enabled);
    resolveDependenciesCheck_->setEnabled(enabled);
    publishButton_->setEnabled(enabled);
}

}
