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
#include "ores.qt/SyntheticBindingDialog.hpp"
#include "ores.marketdata.api/domain/feed_binding.hpp"
#include "ores.marketdata.api/messaging/feed_binding_protocol.hpp"
#include "ores.qt/MessageBoxHelper.hpp"
#include "ores.synthetic.api/messaging/fx_spot_generation_config_protocol.hpp"
#include "ores.synthetic.api/messaging/ir_curve_generation_config_protocol.hpp"
#include <QDialogButtonBox>
#include <QFutureWatcher>
#include <QHBoxLayout>
#include <QHeaderView>
#include <QLabel>
#include <QPushButton>
#include <QVBoxLayout>
#include <QtConcurrent>
#include <boost/uuid/random_generator.hpp>
#include <algorithm>
#include <cctype>

namespace ores::qt {

using namespace ores::logging;

namespace {

// Canonical oresmd URIs for the synthetic configs, built the same way the oresmd parser's
// to_uri() emits them (GUI-side mirror -- service-layer code isn't linkable from Qt).
// The FX spot URI host is base+quote concatenated; the IR fixing URI carries
// index/tenor and type=fixing.
std::string to_lower_copy(std::string s) {
    std::transform(s.begin(), s.end(), s.begin(), [](unsigned char c) {
        return std::tolower(c);
    });
    return s;
}

std::string fx_spot_uri(const synthetic::domain::fx_spot_generation_config& cfg) {
    return "oresmd://fx/" + to_lower_copy(cfg.base_currency_code + cfg.quote_currency_code) +
           "?type=quote&quote=spot";
}

std::string ir_fixing_uri(const synthetic::domain::ir_curve_generation_config& cfg) {
    auto uri = "oresmd://ir/" + to_lower_copy(cfg.currency_code) + "?index=" +
               to_lower_copy(cfg.index_family);
    if (!cfg.tenor.empty())
        uri += "&tenor=" + to_lower_copy(cfg.tenor);
    uri += "&type=fixing";
    return uri;
}

}

SyntheticBindingDialog::SyntheticBindingDialog(ClientManager* clientManager,
                                               const std::string& username,
                                               QWidget* parent)
    : QDialog(parent)
    , clientManager_(clientManager)
    , username_(username)
    , table_(new QTableWidget(this))
    , createButton_(new QPushButton(tr("Create bindings"), this)) {

    setWindowTitle(tr("Create bindings from synthetic feeds"));
    setMinimumSize(700, 400);

    auto* layout = new QVBoxLayout(this);

    layout->addWidget(
        new QLabel(tr("Select synthetic FX/IR curve feeds to create feed bindings for. "
                      "Rows already bound are pre-checked and will be skipped."),
                   this));

    table_->setColumnCount(4);
    table_->setHorizontalHeaderLabels({tr(""), tr("Type"), tr("oresmd URI"), tr("Source name")});
    table_->horizontalHeader()->setSectionResizeMode(0, QHeaderView::ResizeToContents);
    table_->horizontalHeader()->setSectionResizeMode(1, QHeaderView::ResizeToContents);
    table_->horizontalHeader()->setSectionResizeMode(2, QHeaderView::Stretch);
    table_->horizontalHeader()->setSectionResizeMode(3, QHeaderView::Stretch);
    table_->setSelectionBehavior(QAbstractItemView::SelectRows);
    table_->setEditTriggers(QAbstractItemView::NoEditTriggers);
    table_->verticalHeader()->setVisible(false);
    layout->addWidget(table_);

    auto* btnRow = new QHBoxLayout;
    auto* selectAll = new QPushButton(tr("Select all"), this);
    auto* selectNone = new QPushButton(tr("Select none"), this);
    btnRow->addWidget(selectAll);
    btnRow->addWidget(selectNone);
    btnRow->addStretch();
    layout->addLayout(btnRow);

    auto* box = new QDialogButtonBox(this);
    createButton_->setDefault(true);
    box->addButton(createButton_, QDialogButtonBox::AcceptRole);
    box->addButton(QDialogButtonBox::Cancel);
    layout->addWidget(box);

    connect(selectAll, &QPushButton::clicked, this, &SyntheticBindingDialog::onSelectAllClicked);
    connect(selectNone, &QPushButton::clicked, this, &SyntheticBindingDialog::onSelectNoneClicked);
    connect(createButton_, &QPushButton::clicked, this, &SyntheticBindingDialog::onCreateClicked);
    connect(box, &QDialogButtonBox::rejected, this, &QDialog::reject);

    loadConfigs();
}

void SyntheticBindingDialog::loadConfigs() {
    table_->setEnabled(false);
    createButton_->setEnabled(false);

    auto* cm = clientManager_;
    QPointer<SyntheticBindingDialog> self = this;

    struct Result {
        bool ok = false;
        std::vector<BindableConfig> configs;
        std::vector<std::string> existing_source_names;
    };
    auto task = [cm]() -> Result {
        std::vector<BindableConfig> configs;

        // Fetch already-bound source names first so the table can pre-tick rows the caller
        // doesn't need to know about -- keeps this dialog self-contained (no dependency on
        // whatever list window/model happens to be open) rather than requiring the caller to
        // pass a possibly-stale snapshot in.
        std::vector<std::string> existing_source_names;
        marketdata::messaging::get_feed_bindings_request fb_req{.offset = 0, .limit = 1000};
        auto fb_resp = cm->process_authenticated_request(fb_req);
        if (fb_resp && fb_resp->success)
            for (const auto& b : fb_resp->feed_bindings)
                existing_source_names.push_back(b.source_name);

        synthetic::messaging::get_fx_spot_generation_configs_request fx_req;
        auto fx_resp = cm->process_authenticated_request(fx_req);
        if (!fx_resp)
            return {.ok = false};
        for (const auto& cfg : fx_resp->fx_spot_generation_configs)
            configs.push_back(
                {"FX", fx_spot_uri(cfg), cfg.source_name, marketdata::domain::asset_class::fx});

        // IR is additive to the pre-existing FX-only flow: if the IR service is
        // unavailable, log and keep going with the FX rows already fetched rather than
        // failing the whole dialog closed (FX binding must keep working regardless of IR's
        // availability).
        synthetic::messaging::get_ir_curve_generation_configs_request ir_req;
        auto ir_resp = cm->process_authenticated_request(ir_req);
        if (!ir_resp) {
            BOOST_LOG_SEV(lg(), warn) << "Could not load IR curve generation configs; "
                                         "showing FX rows only.";
        } else {
            for (const auto& cfg : ir_resp->ir_curve_generation_configs)
                configs.push_back(
                    {"IR", ir_fixing_uri(cfg), cfg.source_name, marketdata::domain::asset_class::rates});
        }

        return Result{.ok = true,
                      .configs = std::move(configs),
                      .existing_source_names = std::move(existing_source_names)};
    };

    auto* watcher = new QFutureWatcher<Result>(this);
    connect(watcher, &QFutureWatcher<Result>::finished, this, [self, watcher]() {
        auto [ok, configs, existing_source_names] = watcher->result();
        watcher->deleteLater();
        if (!self)
            return;
        if (!ok) {
            MessageBoxHelper::critical(self,
                                       self->tr("Load failed"),
                                       self->tr("Could not load synthetic FX/IR curve configs."));
            self->reject();
            return;
        }
        self->configs_ = std::move(configs);
        self->existingSourceNames_ = std::move(existing_source_names);
        self->populateTable(self->configs_);
        self->table_->setEnabled(true);
        self->createButton_->setEnabled(true);
    });
    watcher->setFuture(QtConcurrent::run(task));
}

void SyntheticBindingDialog::populateTable(const std::vector<BindableConfig>& configs) {
    table_->setRowCount(static_cast<int>(configs.size()));

    for (int row = 0; row < static_cast<int>(configs.size()); ++row) {
        const auto& cfg = configs[row];

        auto* chk = new QTableWidgetItem;
        chk->setFlags(Qt::ItemIsUserCheckable | Qt::ItemIsEnabled);

        const bool alreadyBound =
            std::ranges::find(existingSourceNames_, cfg.source_name) != existingSourceNames_.end();
        chk->setCheckState(alreadyBound ? Qt::Checked : Qt::Unchecked);
        if (alreadyBound)
            chk->setToolTip(tr("Already bound — will be skipped"));

        table_->setItem(row, 0, chk);
        table_->setItem(row, 1, new QTableWidgetItem(QString::fromStdString(cfg.kind)));
        table_->setItem(row, 2, new QTableWidgetItem(QString::fromStdString(cfg.oresmd_uri)));
        table_->setItem(row, 3, new QTableWidgetItem(QString::fromStdString(cfg.source_name)));
    }
}

void SyntheticBindingDialog::onSelectAllClicked() {
    for (int r = 0; r < table_->rowCount(); ++r)
        if (auto* chk = table_->item(r, 0))
            chk->setCheckState(Qt::Checked);
}

void SyntheticBindingDialog::onSelectNoneClicked() {
    for (int r = 0; r < table_->rowCount(); ++r)
        if (auto* chk = table_->item(r, 0))
            chk->setCheckState(Qt::Unchecked);
}

void SyntheticBindingDialog::onCreateClicked() {
    std::vector<BindableConfig> selected;
    for (int r = 0; r < table_->rowCount(); ++r) {
        auto* chk = table_->item(r, 0);
        if (!chk || chk->checkState() != Qt::Checked)
            continue;
        if (r >= static_cast<int>(configs_.size()))
            continue;
        const auto& cfg = configs_[r];
        // Skip rows that already have a binding.
        if (std::ranges::find(existingSourceNames_, cfg.source_name) != existingSourceNames_.end())
            continue;
        selected.push_back(cfg);
    }

    if (selected.empty()) {
        MessageBoxHelper::information(
            this, tr("Nothing to create"), tr("No new synthetic feeds selected."));
        return;
    }

    createBindings(selected);
}

void SyntheticBindingDialog::createBindings(const std::vector<BindableConfig>& selected) {
    createButton_->setEnabled(false);
    table_->setEnabled(false);

    auto* cm = clientManager_;
    const std::string username = username_;
    QPointer<SyntheticBindingDialog> self = this;

    using SaveResult = std::vector<std::pair<std::string, bool>>;
    auto task = [cm, username, selected]() -> SaveResult {
        SaveResult results;
        for (const auto& cfg : selected) {
            marketdata::domain::feed_binding b;
            b.id = boost::uuids::random_generator()();
            b.oresmd_uri = cfg.oresmd_uri;
            b.source_name = cfg.source_name;
            b.asset_class = cfg.asset_class;
            b.enabled = true;
            b.performed_by = username;
            b.change_reason_code = "system.new_record";
            b.change_commentary = "Created from synthetic feed binding dialog";

            auto req = marketdata::messaging::save_feed_binding_request::from(std::move(b));

            auto resp = cm->process_authenticated_request(req);
            const bool ok = resp && resp->success;
            results.push_back({cfg.oresmd_uri, ok});
        }
        return results;
    };

    auto* watcher = new QFutureWatcher<SaveResult>(this);
    connect(watcher, &QFutureWatcher<SaveResult>::finished, this, [self, watcher]() {
        auto results = watcher->result();
        watcher->deleteLater();
        if (!self)
            return;

        int ok = 0, failed = 0;
        for (const auto& [uri, success] : results) {
            if (success)
                ++ok;
            else
                ++failed;
        }

        self->bindingsCreated_ = ok;

        if (failed == 0) {
            self->accept();
        } else {
            MessageBoxHelper::warning(
                self,
                self->tr("Partial success"),
                self->tr("Created %1 binding(s); %2 failed. Check logs for details.")
                    .arg(ok)
                    .arg(failed));
            if (ok > 0)
                self->accept();
            else
                self->reject();
        }
    });
    watcher->setFuture(QtConcurrent::run(task));
}

}
