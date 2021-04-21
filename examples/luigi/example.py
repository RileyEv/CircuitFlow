import luigi
import time

# import luigi.contrib.sqla
import pandas as pd


class Streams(luigi.ExternalTask):

    month = luigi.Parameter()
    user_id = luigi.Parameter()  # Just ignored as i only have 1 copy of files

    def output(self):
        return luigi.LocalTarget(f"../data/{self.month}.csv")


class AggregateArtists(luigi.Task):

    months = luigi.ListParameter()
    user_id = luigi.Parameter()

    def output(self):
        return luigi.LocalTarget(
            "output/artist_streams_{}_{}.csv".format(
                self.user_id, "-".join(self.months)
            ),
            format=luigi.format.Nop,
        )

    def requires(self):
        return [Streams(month, self.user_id) for month in self.months]

    def run(self):
        main_df = None
        for t in self.input():
            with t.open("r") as in_file:
                df = pd.read_csv(in_file)
                if main_df is None:
                    main_df = df
                else:
                    main_df = main_df.append(df)

        counts = (
            main_df.groupby("Artist Name")
            .count()["Apple Id Number"]
            .sort_values(ascending=False)
            .rename("Count")
        )

        with self.output().open("w") as out_file:
            counts.to_csv(out_file)


class AggregateSongs(luigi.Task):

    months = luigi.ListParameter()
    user_id = luigi.Parameter()

    def output(self):
        return luigi.LocalTarget(
            "output/song_streams_{}_{}.csv".format(self.user_id, "-".join(self.months)),
            format=luigi.format.Nop,
        )

    def requires(self):
        return [Streams(month, self.user_id) for month in self.months]

    def run(self):
        main_df = None
        for t in self.input():
            with t.open("r") as in_file:
                df = pd.read_csv(in_file)
                if main_df is None:
                    main_df = df
                else:
                    main_df = main_df.append(df)

        counts = (
            main_df.groupby(["Artist Name", "Content Name"])
            .count()["Apple Id Number"]
            .sort_values(ascending=False)
            .rename("Count")
        )

        with self.output().open("w") as out_file:
            counts.to_csv(out_file)


class Top10Artists(luigi.Task):

    months = luigi.ListParameter()
    user_id = luigi.Parameter()

    def requires(self):
        return AggregateArtists(self.months, self.user_id)

    def output(self):
        return luigi.LocalTarget(
            "output/top_artists_{}_{}.csv".format(self.user_id, "-".join(self.months)),
            format=luigi.format.Nop,
        )

    def run(self):
        with self.input().open("r") as in_file:
            df = pd.read_csv(in_file)

        df = df.iloc[:, :10]
        with self.output().open("w") as out_file:
            df.to_csv(out_file, index=False)


class Top10Songs(luigi.Task):

    months = luigi.ListParameter()
    user_id = luigi.Parameter()

    def requires(self):
        return AggregateSongs(self.months, self.user_id)

    def output(self):
        return luigi.LocalTarget(
            "output/top_songs_{}_{}.csv".format(self.user_id, "-".join(self.months)),
            format=luigi.format.Nop,
        )

    def run(self):
        with self.input().open("r") as in_file:
            df = pd.read_csv(in_file)

        df = df.iloc[:, :10]
        with self.output().open("w") as out_file:
            df.to_csv(out_file, index=False)


# TODO: Replace with sqlite - luigi.contrib.sqla.CopyToTable
# https://luigi.readthedocs.io/en/stable/api/luigi.contrib.sqla.html
# class ArtistToplistToDatabase(luigi.contrib.postgres.CopyToTable):

#     date_interval = luigi.DateIntervalParameter()

#     host = "localhost"
#     database = "toplists"
#     user = "luigi"
#     password = "abc123"
#     table = "top10"

#     columns = [
#         ("date_from", "DATE"),
#         ("date_to", "DATE"),
#         ("artist", "TEXT"),
#         ("streams", "INT"),
#     ]

#     def requires(self):
#         return Top10Artists(self.date_interval)


if __name__ == "__main__":
    months = ["jan", "feb", "mar"]
    tstart = time.time()
    luigi.build(
        [
            task
            for user in ["0", "1", "2", "3", "4", "5", "6", "7", "8", "9"]
            for task in [Top10Artists(months, user), Top10Songs(months, user)]
        ],
        workers=4,
        local_scheduler=True,
    )
    tend = time.time()
    print("Total Runtime (s): {:.6f}".format(tend - tstart))
